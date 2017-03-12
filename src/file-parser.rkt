#lang racket

(require data/monad
         data/applicative
         megaparsack megaparsack/text
         
         "string-utils.rkt"
         "macro-utils.rkt"
         "parser-components.rkt"
         "parsed-utils.rkt"
         "parse-error-struct-def.rkt"
         "global-parameters.rkt"
         "print-utils.rkt")

(provide parse-file-string
         (all-from-out "parsed-utils.rkt"))

(define include/p
  (lockhead-parser (keyword)
                   ((syntax/p (string-ci/p "include")))
                   (blank-space+/p
                    [file-name-syntax <- (syntax/p file-name-string/p)]
                    (pure (parsed-auto-id 'INCLUDE
                                          (list file-name-syntax)
                                          (list (remove-head-tail-space (syntax->datum file-name-syntax)))
                                          (hash 'keyword keyword)
                                          null)))))

(define tab-space=/p
  (lockhead-parser (keyword)
                   ((syntax/p (string-ci/p "tab-space")))
                   (blank-space/p
                    (char/p #\=)
                    blank-space/p
                    [space-syntax <- (syntax/p (guard/p integer/p (λ (x) (<= x 8))
                                                        "tab space exceeds maximum : 8"))]
                    (pure (parsed-auto-id 'TAB-SPACE
                                          (list space-syntax)
                                          null
                                          (hash 'keyword keyword
                                                'tab-space (syntax->datum space-syntax))
                                          null)))))

(define comment/p
  (do (or/p (try/p (string/p "//"))
            (char/p   #\#))
      comment-string/p
      (pure 'COMMENT)))

(define-syntax (many-in-block/p stx)
  (syntax-case stx (time-related task-or-remark)
    [(_ (prepend ...) () (append ...))                         #'(many-in-block/p prepend ...
                                                                                  append  ...)]
    [(_ (prepend ...) (time-related other ...) (append ...))   #'(many-in-block/p (prepend ...)
                                                                                  (other ...)
                                                                                  (+year-block/p
                                                                                   +month-block/p
                                                                                   +week-block/p
                                                                                   +day-block/p
                                                                                   +time-block/p

                                                                                   translation/p
                                                                                   
                                                                                   +from+to-block/p
                                                                                   +recur-block/p
                                                                                
                                                                                   append
                                                                                   ...))]
    [(_ (prepend ...) (task-or-remark other ...) (append ...)) #'(many-in-block/p (prepend ...)
                                                                                  (other ...)
                                                                                  (~task-block/p
                                                                                   *remark-block/p

                                                                                   append
                                                                                   ...))]
    [(_ parser ...)                                            #'(many/p (or/p (try/p comment/p)
                                                                               (try/p blank-space/p)
                                                                               (try/p line-end/p)

                                                                               parser
                                                                               ...))]))

(define-syntax (one-of-block/p stx)
  (syntax-case stx ()
    [(_ parser ...) #'(do (many/p (or/p (try/p comment/p)
                                        (try/p blank-space/p)
                                        line-end/p))
                          (or/p (try/p parser)
                                ...))]))

(define +from+to/p
  (lockhead-parser (keyword)
                   ((syntax/p (string-ci/p "+from-")))
                   (;(char/p #\-)
                    [unit <- (syntax/p (one-of-strings-ci/p "year"
                                                            "month"
                                                            "week"
                                                            "day"
                                                            ;"date"
                                                            "time"))]
                    blank-space+/p
                    [from <- (syntax/p from-string/p)]
                    blank-space-nonstrict/p
                    (string-ci/p "+to")
                    blank-space+/p
                    [to   <- (syntax/p time-block-head-string/p)]
                    (pure (parsed-auto-id '+FROM+TO
                                          (list unit from to)
                                          (list (syntax->datum unit)
                                                (remove-head-tail-space (syntax->datum from))
                                                (remove-head-tail-space (syntax->datum to)))
                                          (let ([keyword-str (syntax->datum keyword)]
                                                [unit-str    (syntax->datum unit)])
                                            (hash 'keyword (datum->syntax keyword (format "~a~a" keyword-str unit-str) keyword)))
                                          null)))))

(define +from+to-block/p
  (do [head <- +from+to/p]
      blank-space-nonstrict/p
      [info <- time-add-info-list/p]
      blank-space-nonstrict/p
      [brac-ht <- (brac/p (many-in-block/p () (time-related task-or-remark) (unrecognized/p)))]
      (pure (parsed-auto-id (parsed-token    head)
                            (parsed-syntaxes head)
                            (parsed-strings  head)
                            (hash-set*       (parsed-attrs head)
                                             'info       info
                                             'start-brac (hash-ref brac-ht 'start-brac)
                                             'end-brac   (hash-ref brac-ht 'end-brac))
                            (hash-ref brac-ht 'chunk)))))

(define translation/p
  (lockhead-parser (start-square)
                   ((syntax/p (string-ci/p "[[")))
                   (blank-space/p
                    [original <- (syntax/p translation-original-string/p)]
                    blank-space/p
                    [brac-ht <- (or/p (try/p (do (brac/p blank-space-nonstrict/p)
                                                 (pure (hash 'chunk      null
                                                             'start-brac #f
                                                             'end-brac   #f))))
                                      (try/p (brac/p (comma-separated-parse/p (syntax/p resolved@/p)
                                                                              (syntax/p start@/p)
                                                                              unrecognized/p)))
                                      (pure  (hash 'chunk      null
                                                   'start-brac #f
                                                   'end-brac   #f)))]
                    blank-space/p
                    [end-square <- (syntax/p (string-ci/p "]]"))]
                    blank-space-nonstrict/p
                    (string-ci/p "->")
                    blank-space-nonstrict/p
                    [res <- (one-of-block/p +year-block/p
                                            +month-block/p
                                            +day-block/p)]
                    (pure (cond
                            [(parsed? res) (parsed (parsed-token    res)
                                                   (parsed-id       res)
                                                   (parsed-syntaxes res)
                                                   (parsed-strings  res)
                                                   (hash-set*       (parsed-attrs res)
                                                                    'original           original
                                                                    'original-str       (remove-head-tail-space (syntax->datum original))
                                                                    'translation        (hash 
                                                                                         'attrs        (hash-ref brac-ht 'chunk)
                                                                                         'start-brac   (hash-ref brac-ht 'start-brac)
                                                                                         'end-brac     (hash-ref brac-ht 'end-brac)
                                                                                         'start-square start-square
                                                                                         'end-square   end-square))
                                                   (parsed-content  res))]
                            [else           res])))))

(define +year-info/p
  (add-info/p "+year"))

(define +month-info/p
  (add-info/p "+month"))

(define +week-info/p
  (add-info/p "+week"))

(define +day-info/p
  (add-info/p "+day"))

(define +time-info/p
  (add-info/p "+time"))

(define-syntax (time-add-info-list/p stx)
  (syntax-case stx ()
    [_ #'(add-info-list/p +year-info/p
                          +month-info/p
                          +week-info/p
                          +day-info/p
                          +time-info/p
                          (unrecognized/p add-info-string/p))]))

(define +year-block/p
  (lockhead-parser (keyword)
                   ((syntax/p (string-ci/p "+year")))
                   ((label/p "whitespace"
                             blank-space+/p)
                    [year <- (syntax/p time-block-head-string/p)]
                    blank-space-nonstrict/p
                    [info <- time-add-info-list/p]
                    blank-space-nonstrict/p
                    [brac-ht <- (brac/p (many-in-block/p () (time-related task-or-remark) (unrecognized/p)))]
                    (pure (parsed-auto-id '+YEAR
                                          (list year)
                                          (list (remove-head-tail-space (syntax->datum year)))
                                          (hash 'keyword    keyword
                                                'info       info
                                                'start-brac (hash-ref brac-ht 'start-brac)
                                                'end-brac   (hash-ref brac-ht 'end-brac))
                                          (hash-ref brac-ht 'chunk))))))

(define +month-block/p
  (lockhead-parser (keyword)
                   ((syntax/p (string-ci/p "+month")))
                   ((label/p "whitespace"
                             blank-space+/p)
                    [month <- (syntax/p time-block-head-string/p)]
                    blank-space-nonstrict/p
                    [info <- time-add-info-list/p]
                    blank-space-nonstrict/p
                    [brac-ht <- (brac/p (many-in-block/p () (time-related task-or-remark) (unrecognized/p)))]
                    (pure (parsed-auto-id '+MONTH
                                          (list month)
                                          (list (remove-head-tail-space (syntax->datum month)))
                                          (hash 'keyword    keyword
                                                'info       info
                                                'start-brac (hash-ref brac-ht 'start-brac)
                                                'end-brac   (hash-ref brac-ht 'end-brac))
                                          (hash-ref brac-ht 'chunk))))))

(define +week-block/p
  (lockhead-parser (keyword)
                   ((syntax/p (string-ci/p "+week")))
                   ((label/p "whitespace"
                             blank-space+/p)
                    [week <- (syntax/p time-block-head-string/p)]
                    blank-space-nonstrict/p
                    [info <- time-add-info-list/p]
                    blank-space-nonstrict/p
                    [brac-ht <- (brac/p (many-in-block/p () (time-related task-or-remark) (unrecognized/p)))]
                    (pure (parsed-auto-id '+WEEK
                                          (list week)
                                          (list (remove-head-tail-space (syntax->datum week)))
                                          (hash 'keyword    keyword
                                                'info       info
                                                'start-brac (hash-ref brac-ht 'start-brac)
                                                'end-brac   (hash-ref brac-ht 'end-brac))
                                          (hash-ref brac-ht 'chunk))))))

(define +day-block/p
  (lockhead-parser (keyword)
                   ((syntax/p (string-ci/p "+day")))
                   ((label/p "whitespace"
                             blank-space+/p)
                    [day <- (syntax/p time-block-head-string/p)]
                    blank-space-nonstrict/p
                    [info <- time-add-info-list/p]
                    blank-space-nonstrict/p
                    [brac-ht <- (brac/p (many-in-block/p () (time-related task-or-remark) (unrecognized/p)))]
                    (pure (parsed-auto-id '+DAY
                                          (list day)
                                          (list (remove-head-tail-space (syntax->datum day)))
                                          (hash 'keyword    keyword
                                                'info       info
                                                'start-brac (hash-ref brac-ht 'start-brac)
                                                'end-brac   (hash-ref brac-ht 'end-brac))
                                          (hash-ref brac-ht 'chunk))))))

(define +time-block/p
  (lockhead-parser (keyword)
                   ((syntax/p (string-ci/p "+time")))
                   ((label/p "whitespace"
                             blank-space+/p)
                    [time <- (syntax/p time-block-head-string/p)]
                    blank-space-nonstrict/p
                    [info <- time-add-info-list/p]
                    blank-space-nonstrict/p
                    [brac-ht <- (brac/p (many-in-block/p () (time-related task-or-remark) (unrecognized/p)))]
                    (pure (parsed-auto-id '+TIME
                                          (list time)
                                          (list (remove-head-tail-space (syntax->datum time)))
                                          (hash 'keyword    keyword
                                                'info       info
                                                'start-brac (hash-ref brac-ht 'start-brac)
                                                'end-brac   (hash-ref brac-ht 'end-brac))
                                          (hash-ref brac-ht 'chunk))))))

(define recur-start@/p
  (add-info/p "start@"))

(define recur-end@/p
  (add-info/p "end@"))

(define +recur-block/p
  (lockhead-parser (keyword)
                   ((syntax/p (string-ci/p "+recur")))
                   (blank-space+/p
                    [cycle <- (syntax/p (or/p (try/p (one-of-strings-ci/p "yearly"
                                                                          "monthly"
                                                                          "weekly"
                                                                          "daily"
                                                                          "hourly"))
                                              (do (string-ci/p "every")
                                                  blank-space+/p
                                                  [interval <- integer/p]
                                                  blank-space+/p
                                                  [unit     <- (one-of-strings-ci/p "years"   "year"
                                                                                    "months"  "month"
                                                                                    "days"    "day"
                                                                                    "hours"   "hour"
                                                                                    "minutes" "minute")]
                                                  (pure (list interval unit)))))]
                    blank-space-nonstrict/p
                    [info <- (add-info-list/p recur-start@/p
                                              recur-end@/p
                                              (unrecognized/p add-info-string/p))]
                    blank-space-nonstrict/p
                    [brac-ht <- (brac/p (many-in-block/p () (time-related task-or-remark) (unrecognized/p)))]
                    (pure (parsed-auto-id '+RECUR
                                          (list cycle)
                                          (list (syntax->datum cycle))
                                          (hash 'cycle      (let ([cycle-datum (syntax->datum cycle)])
                                                              (if (string? cycle-datum)
                                                                  (string-downcase (remove-head-tail-space cycle-datum))
                                                                  cycle-datum))
                                                'keyword    keyword
                                                'info       info
                                                'start-brac (hash-ref brac-ht 'start-brac)
                                                'end-brac   (hash-ref brac-ht 'end-brac))
                                          (hash-ref brac-ht 'chunk))))))

(define ~task-block/p
  (lockhead-parser (keyword)
                   ((syntax/p (string-ci/p "~")))
                   ([finished? <- (do (or/p (try/p (do (char/p #\x)
                                                       (pure #t)))
                                            (pure #f)))]
                    (label/p "whitespace"
                             blank-space+/p)
                    [name <- (syntax/p block-head-string/p)]
                    blank-space-nonstrict/p
                    [brac-ht <- (or/p (brac/p (many-in-block/p () (time-related) (~task-block/p
                                                                                  
                                                                                  +tags-block/p
                                                                                  @place-block/p
                                                                                  ?desc-block/p
                                                                                  !dep-block/p
                                                                                  <<history-block/p
                                                                                  
                                                                                  unrecognized/p)))
                                      (pure (hash 'chunk      null
                                                  'start-brac #f
                                                  'end-brac   #f)))]
                    (pure (parsed-auto-id '~TASK
                                          (list name)
                                          (list (remove-head-tail-space (syntax->datum name)))
                                          (hash 'finished?  finished?
                                                'keyword    keyword
                                                'start-brac (hash-ref brac-ht 'start-brac)
                                                'end-brac   (hash-ref brac-ht 'end-brac))
                                          (hash-ref brac-ht 'chunk))))))

(define *remark-block/p
  (lockhead-parser (keyword)
                   ((syntax/p (string-ci/p "*")))
                   ((label/p "whitespace"
                             blank-space+/p)
                    [name <- (syntax/p block-head-string/p)]
                    blank-space-nonstrict/p
                    [brac-ht <- (or/p (brac/p (many-in-block/p () (time-related) (*remark-block/p
                                                                                  
                                                                                  +tags-block/p
                                                                                  @place-block/p
                                                                                  ?desc-block/p
                                                                                  !dep-block/p
                                                                                  
                                                                                  unrecognized/p)))
                                      (pure (hash 'chunk      null
                                                  'start-brac #f
                                                  'end-brac   #f)))]
                    (pure (parsed-auto-id '*REMARK
                                          (list name)
                                          (list (remove-head-tail-space (syntax->datum name)))
                                          (hash 'keyword    keyword
                                                'start-brac (hash-ref brac-ht 'start-brac)
                                                'end-brac   (hash-ref brac-ht 'end-brac))
                                          (hash-ref brac-ht 'chunk))))))

(define +tags-block/p
  (lockhead-parser ((string-ci/p "+tags"))
                   (blank-space-nonstrict+/p
                    [brac-ht <- (brac/p comma-separated-strings/p)]
                    (pure (parsed-auto-id '+TAGS
                                          null
                                          null
                                          (hash 'start-brac (hash-ref brac-ht 'start-brac)
                                                'end-brac   (hash-ref brac-ht 'end-brac))
                                          (hash-ref brac-ht 'chunk))))))

(define @place-block/p
  (lockhead-parser ((string-ci/p "@place"))
                   (blank-space-nonstrict+/p
                    [brac-ht <- (brac/p comma-separated-strings/p)]
                    (pure (parsed-auto-id '@PLACE
                                          null
                                          null
                                          (hash 'start-brac (hash-ref brac-ht 'start-brac)
                                                'end-brac   (hash-ref brac-ht 'end-brac))
                                          (hash-ref brac-ht 'chunk))))))

(define ?desc-block/p
  (lockhead-parser ((string-ci/p "?desc"))
                   (blank-space-nonstrict+/p
                    [brac-ht <- (brac/p desc-string/p)]
                    (pure (parsed-auto-id '?DESC
                                          null
                                          null
                                          (hash 'start-brac (hash-ref brac-ht 'start-brac)
                                                'end-brac   (hash-ref brac-ht 'end-brac))
                                          (remove-extra-indent
                                           (string->lines
                                            (remove-head-tail-empty-lines (hash-ref brac-ht 'chunk))))
                                          )))))

(define !dep-block/p
  (lockhead-parser ((string-ci/p "!dep"))
                   (blank-space-nonstrict+/p
                    [brac-ht <- (brac/p comma-separated-strings/p)]
                    (pure (parsed-auto-id '!DEP
                                          null
                                          null
                                          (hash 'start-brac (hash-ref brac-ht 'start-brac)
                                                'end-brac   (hash-ref brac-ht 'end-brac))
                                          (hash-ref brac-ht 'chunk))))))

(define <<history-block/p
  (lockhead-parser ((string-ci/p "<<history"))
                   (blank-space-nonstrict+/p
                    [brac-ht <- (or/p (try/p (do (brac/p blank-space-nonstrict/p)
                                                 (pure null)))
                                      (try/p (brac/p (comma-separated-parse/p (syntax/p done-time/p)
                                                                              unrecognized/p)))
                                      (pure (hash 'chunk      null
                                                  'start-brac #f
                                                  'end-brac   #f)))]
                    (pure (parsed-auto-id '<<HISTORY
                                          null
                                          null
                                          (hash 'start-brac (hash-ref brac-ht 'start-brac)
                                                'end-brac   (hash-ref brac-ht 'end-brac))
                                          (hash-ref brac-ht 'chunk))))))

(date-time-parser-gen (done-time/p "done-"      'DONE-TIME)
                      (resolved@/p "resolved@-" 'RESOLVED@)
                      (start@/p    "start@-"    'START@))

(define file/p
  (many-in-block/p (include/p tab-space=/p)
                   (time-related task-or-remark)
                   ((unrecognized/p any-string/p))))

(define/contract (print-srclocs srclocs)
  (-> (listof srcloc?)
      void?)
  (for ([loc srclocs])
    (display (if (file-name)
                 (format "file : ~a, line : ~a, column : ~a "
                         (file-name)
                         (srcloc-line   loc)
                         (srcloc-column loc))
                 (format "line : ~a, column : ~a "
                         (srcloc-line   loc)
                         (srcloc-column loc))))))

(define (print-expected expecteds)
  (define (print-list lst [first #t])
    (match lst
      [(list)            (void)]
      [(list e)          (display
                          (format (if first
                                      "~a"
                                      ", or ~a") e))]
      [(list e rest ...) (begin
                           (display
                            (format (if first
                                        "~a"
                                        ", ~a") e))
                           (print-list rest #f))]))
  (display "expected : ")
  (print-list expecteds)
  (newline))

(define (show-error error)
  (match error
    [(struct exn:fail:read:megaparsack
       (_ _ srclocs unexpected expected)) (begin
                                            (print-srclocs srclocs)
                                            (display "-> ")
                                            (if (empty? expected)
                                                (displayln
                                                 (format "unexpected/unrecognized : ~a"
                                                         unexpected))
                                                (print-expected expected))
                                            #f)]))

(define/contract (parse-file-string str)
  (-> string?
      (or/c list? parsed? symbol?
            parse-error?))
  (with-handlers ([exn:fail:read:megaparsack? package-error])
    (cleanup-parsed
     (parse-result!
      (parse-string file/p str)))))

(module+ test
  
;  (with-handlers ([exn:fail:read:megaparsack? show-error])
;    (print-parsed
;     (cleanup-parsed
;      (parse-result!
;       (parse-string +dura-block/p "+dura 8-9am { ~ task }
;")))))

  (newline)

  (with-handlers ([exn:fail:read:megaparsack? show-error])
    (print-parsed
     (cleanup-parsed
      (parse-result!
       (parse-string done-time/p "done-2017-01-23.17:20")))))

  (newline)

  (define test-file "//

# o
// test
include abc
tab-space = 4
// this-year = 2017
// this-month = 2
// today      = 8

+time 13:21 {
}

+month July {
  [[ +day 1st Sunday { resolved@-2017-02-03.11:11, start@-2017-02-03.10:10 }]] -> // comment
  +day 2 {
    * holiday 123
  }
  [[ +day 2nd Sunday {}]] -> +day 3 {
  }
}

~ task1 {
 ~x subtask 1 {
      ~ sub subtask 1
      ~ sub subtask 2

      +tags {1,
             2
      } // silly remark

      @place { CSIT }

      ?desc {
        header
          sub 1 # abc
          sub 2
      }
      !dep { abc }

      <<history {
          done-2017-01-23.17:20,
          done-2017-01-30.17:21
      }
   }
}


+from-month july +to dec {
  * school work
}"
    )
  (with-handlers ([exn:fail:read:megaparsack? show-error])
    (define file-parse-result
      (cleanup-parsed
       (parse-result!
        (parse-string file/p test-file))
       ))

    (print-parsed file-parse-result)

    (newline)

    (print-branch-lists
     (parsed->branch-lists file-parse-result))
    )
  )
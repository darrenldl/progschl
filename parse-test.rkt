#lang racket

(require data/monad)
(require data/applicative)

(require megaparsack megaparsack/text)

;(define (cleanup-parsed lst)
;  (define (remove-ignored lst)
;    (match lst
;      [(list)                    (list)]
;      [(list 'LINE-END    b ...) (remove-ignored b)]
;      [(list 'BLANK-SPACE b ...) (remove-ignored b)]
;      [(list 'COMMENT     b ...) (remove-ignored b)]
;      [(list a            b ...) (cons (remove-ignored a) (remove-ignored b))]
;      [a                         a]))
;  
;  (define (delist lst)
;    (match lst
;      [(list)              (list)]  ; preserve empty list
;      [(list (list a ...)) (delist a)]
;      [a                   a]))
;  
;  (delist (remove-ignored lst)))

;(define (remove-head-tail-space str)
;  (let* ([tail-removed (regexp-replace #rx"[ \t]*$" str "")]
;         [head-removed (regexp-replace #rx"^[ \t]*" tail-removed "")])
;    head-removed))

;(define strict-space/p
;  (satisfy/p char-blank?))

;(define comma-andor-space/p
;  (do (many/p strict-space/p)
;      (char/p #\,)
;      (many/p strict-space/p)))

;(define escaped-space/p
;  (try/p (do (char/p #\\)
;             strict-space/p)))

;(define escaped-brac/p
;  (or/p (try/p (do (char/p #\\)
;                   (char/p #\{)))
;        (try/p (do (char/p #\\)
;                   (char/p #\})))))

;(define blank-space/p
;  (do (many/p strict-space/p)
;      (pure 'BLANK-SPACE)))

;(define blank-space+/p
;  (do (many+/p strict-space/p)
;      (pure 'BLANK-SPACE)))

;(define newline/p
;  (guard/p space/p
;           (λ (x)
;             (equal? x #\newline))
;           "#\\newline"))

;(define comment-char/p
;  (satisfy/p (λ (x)
;               (and (char? x)
;                    (not (equal? x #\newline))))))

;(define comment-string/p
;  (many/p comment-char/p))

;(define any-char/p
;  (or/p (satisfy/p (λ (x)
;                     (and (char? x)
;                          (not (equal? x #\newline))
;                          (not (equal? x #\\))
;                          (not (equal? x #\{))
;                          (not (equal? x #\})))))
;        escaped-brac/p))

;(define any-string/p
;  (do [list <- (many/p any-char/p)]
;      (pure (list->string list))))

;(define file-name-char/p
;  (or/p letter/p
;        digit/p
;        symbolic/p
;        escaped-space/p
;        (char-in/p "_-+={}[]/")))

;(define file-name-string/p
;  (do [list <- (many/p file-name-char/p)]
;      (pure (list->string list))))

;(define line-end/p
;  (do newline/p
;      (pure 'LINE-END)))

;(define line-or-file-end/p
;  (or/p line-end/p
;        eof/p))

;(define possible-line-end/p
;  (try/p (char/p #\newline)))

;(define blank-line/p
;  (do (many+/p strict-space/p)
;      line-end/p
;      (pure 'BLANK-LINE)
;      ))

;(define include/p
;  (do (string/p "include")
;      blank-space+/p
;      [file-name <- file-name-string/p]
;      (pure (cons 'INCLUDE (remove-head-tail-space file-name)))))

;(define-syntax (add-try/p-string/p stx)
;  (syntax-case stx ()
;    [(_ str) #'(try/p (string/p str))]))

;(define-syntax (one-of-string/p stx)
;  (syntax-case stx ()
;    [(_ str ...) #'(or/p (add-try/p-string/p str) ...)]))

;(define-syntax (add-dos stx)
;  (syntax-case stx ()
;    [(_ str common-parser parser) #'(do (add-try/p-string/p str)
;                                        common-parser
;                                        parser)]))

;(define test/p
;  (or/p (do (try/p (string/p "abc"))
;            (pure "1"))
;        (do (pure "2"))))

;(define-syntax (many-in-block/p stx)
;  (syntax-case stx ()
;    [(_ str ...) #'(many/p (or/p comment/p
;                                 blank-space/p
;                                 line-end/p
;                                 
;                                 str ...
;
;                                 uncategorised/p))]))

;(define comment/p
;  (do (or/p (string/p "//")
;            (char/p   #\#))
;      comment-string/p
;      (pure 'COMMENT)))

;(define uncategorised/p
;  (guard/p any-string/p
;           (λ (x) #f)))

;(define (brac/p parser)
;  (do (char/p #\{)
;      [chunk <- parser]
;      (char/p #\})
;      (pure chunk)))

;(define-syntax (branched-block-string-prep stx)
;  (syntax-case stx ()
;    [(_ str chunk-parser) #'(do (try/p (string/p str))
;                                blank-space+/p
;                                [chunk <- (brac/p chunk-parser)]
;                                (pure (cons str chunk)))]))

;(define-syntax (branched-block-string/p stx)
;  (syntax-case stx ()
;    [(_ (str ... chunk-parser) ...) #'(or/p (branched-block-string-prep str ...
;                                                                        chunk-parser) ...)]))

(define recur-block/p
  (do (string/p "+recur")
      blank-space+/p
;      [cycle <- (one-of-string/p "yearly" "monthly" "weekly" "daily")]
;      blank-space+/p
;      [chunk <- (brac/p (many-in-block/p named-block/p))]
      [res <- (branched-block-string/p ("yearly"  (many-in-block/p month-in-year-block/p))
                                       ("monthly" (many-in-block/p day-in-month-block/p)))]
      (pure (list 'RECUR res))))

(define month-in-year/p
  (one-of-string/p "Jan" "January"
                   "Feb" "February"
                   "Mar" "March"
                   "Apr" "April"
                   "May"
                   "Jun" "June"
                   "Jul" "July"
                   "Aug" "August"
                   "Sep" "September"
                   "Oct" "October"
                   "Nov" "November"
                   "Dec" "December"))

(define month/p
  (or/p (try/p (string/p "this"))
        month-in-year/p))

(define month-in-year-block/p
  (do (string/p "+month")
      blank-space+/p
      [month <- month-in-year/p]
      blank-space+/p
      [chunk <- (brac/p (many-in-block/p day-in-month-block/p))]
      (pure (list 'MONTH month chunk))))

(define (general-day-in-month/p [max 31])
  (guard/p integer/p
           (λ (x) (and (>  x 0)
                       (<= x max)))))

(define day-in-month-block/p
  (do (string/p "+day")
      blank-space+/p
      [day <- integer/p]
      blank-space+/p
      [chunk <- (brac/p (many-in-block/p named-block/p))]
      (pure (list 'DAY day chunk))))

(define named-block/p
  (do (char/p #\~)
      [finished? <- (do (or/p (try/p (do (char/p #\x)
                                         (pure #t)))
                              (pure #f)))]
      blank-space+/p
      [name <- any-string/p]
      [chunk <- (or/p (try/p (do line-end/p
                                 (pure '())))
                      (do [chunk <- (brac/p (many-in-block/p named-block/p))]
                          (pure chunk)))]
      (pure (list 'NAMED (remove-head-tail-space name) finished? chunk))))

(define file/p
  (many-in-block/p include/p
                 
                 named-block/p
                 recur-block/p
                 ))

(module+ test
  (parse-result! (parse-string file/p "include abc\\ def "))

  (parse-result! (parse-string file/p "//abcjdjiojfe jiodjsijfioe
"))

  (define test-file-str "
+recur yearly { +month Jan {//
//
//
}
}")
  
  (parse-result! (parse-string file/p
                               test-file-str))
  (cleanup-parsed (parse-result! (parse-string file/p
                                               test-file-str)))

  (define test-syntax-obj (parse-result! (parse-string (syntax/p integer/p) "123")))

  (displayln test-syntax-obj)
  (displayln (syntax-span test-syntax-obj))
  (displayln (syntax-original? test-syntax-obj))
  )
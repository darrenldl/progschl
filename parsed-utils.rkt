#lang racket

(provide cleanup-parsed
         print-parsed
         parsed->branch-lists
         parsed-with-token
         (all-from-out "parsed-struct-def.rkt"
                       "parse-error-struct-def.rkt")
         show-parse-error
         parsed-set-attrs
         parsed-time-level)

(require "parsed-struct-def.rkt"
         "parse-error-struct-def.rkt")

(define-syntax (parsed-set-attrs stx)
  (syntax-case stx ()
    [(_ p kv ...) #'(parsed (parsed-token    p)
                            (parsed-id       p)
                            (parsed-syntaxes p)
                            (parsed-strings  p)
                            (hash-set*       (parsed-attrs p)
                                             kv ...)
                            (parsed-content  p))]))

(define/contract (cleanup-parsed in)
  (-> (or/c list? parsed? symbol?)
      (or/c list? parsed? symbol?))
  (define-syntax (unsatisfy-symbol stx)
    (syntax-case stx ()
      [(_ symbol ...) #'(λ (x)
                          (cond
                            [(symbol? x) (and (not (equal? x symbol))
                                              ...)]
                            [else        #t]))]))
  (cond
    [(parsed? in) (parsed (parsed-token    in)
                          (parsed-id       in)
                          (parsed-syntaxes in)
                          (parsed-strings  in)
                          (parsed-attrs    in)
                          (cleanup-parsed (parsed-content in)))]
    [(empty?  in) in]
    [(list?   in) (map cleanup-parsed (filter (unsatisfy-symbol 'BLANK-SPACE
                                                                'BLANK-SPACE-NONSTRICT
                                                                'COMMENT
                                                                'LINE-END)
                                              in))]
    [else         in]))

(define/contract (print-parsed in [indent-level 0])
  (->* ((or/c list? parsed? symbol?))
       (exact-nonnegative-integer?)
       void?)
  (define (print-indent [add-level 0])
    (for ([i (in-range (+ indent-level add-level))])
      (display "  ")))
  (define (print-wi str [add-level 0])
    (print-indent add-level)
    (display str))
  (define (println-wi str [add-level 0])
    (print-wi str add-level)
    (newline))
  (define (print-list-wi lst [add-level 0])
    (if (> (length lst) 0)
        (let-values ([(head last) (split-at lst (- (length lst) 1))])
          (for ([e head])
            (println-wi e add-level))
          (print-wi (first last) add-level))
        (void)))
  (define (println-list-wi lst [add-level 0])
    (if (> (length lst) 0)
        (begin
          (print-list-wi lst add-level)
          (newline))
        (void)))
  (define (println-hash-wi hash [add-level 0])
    (for ([key (hash-keys hash)])
      (println-wi (format "~a -> ~a" key (hash-ref hash key)) add-level)))
  (cond
    [(parsed? in)  (begin
                     (println-wi      "parsed :")
                     (println-wi      (parsed-token    in)              1)
                     (println-wi      (format "id : ~a" (parsed-id in)) 1)
                     (println-wi      "syntaxes :"                      1)
                     (println-list-wi (parsed-syntaxes in)              2)
                     (println-wi      "strings :"                       1)
                     (println-list-wi (parsed-strings  in)              2)
                     (println-wi      "attributes :"                    1)
                     (println-hash-wi (parsed-attrs    in)              2)
                     (println-wi      "content :"                       1)
                     (print-parsed    (parsed-content  in) (+ 2 indent-level)))]
    [(list? in)    (for ([e in])
                     (if (parsed? e)
                         (print-parsed e indent-level)
                         (println-wi   e)))]
    [else          (println-wi in indent-level)]))

(define/contract (parsed-strip-content in)
  (-> parsed?
      parsed?)
  (parsed (parsed-token    in)
          (parsed-id       in)
          (parsed-syntaxes in)
          (parsed-strings  in)
          (parsed-attrs    in)
          null))

(define-syntax (parsed-with-token stx)
  (syntax-case stx (λ)
    [(_ λ tok ...) #'(λ (x)
                       (and (parsed? x)
                            (let ([t (parsed-token x)])
                              (or (equal? t tok)
                                  ...))))]
    [(_ x tok ...) #'(and (parsed? x)
                          (let ([t (parsed-token x)])
                            (or (equal? t tok)
                                ...)))]))

(define/contract (parsed->branch-lists in [acc null] [branch-lists null])
  (->* ((or/c list? parsed? symbol?))
       (list?
        (listof list?))
       (listof list?))
  (define non-branching-parsed?
    (parsed-with-token λ
                       'INCLUDE
                       'TAB-SPACE
                       ;'THIS-YEAR
                       ;'THIS-MONTH
                       ;'TODAY
                       '+TAGS
                       '@PLACE
                       '?DESC
                       '!DEP
                       '<<HISTORY))
  (define (branching-parsed? x)
    (and (parsed? x)
         (not (non-branching-parsed? x))))
  (cond
    [(empty?  in)           (cons (reverse acc) branch-lists)]
    [(list?   in)           (foldr append
                                   branch-lists
                                   (map (λ (x)
                                          (parsed->branch-lists x acc))
                                        in))]
    [(branching-parsed? in) (parsed->branch-lists (parsed-content in)
                                                  (cons (parsed-strip-content in) acc)
                                                  branch-lists)]
    [else                   (parsed->branch-lists null
                                                  (cons in acc)
                                                  branch-lists)]
    ))

(define/contract (show-parse-error error)
  (-> parse-error?
      void?)
  (displayln (let ([fname (parse-error-fname error)])
               (if fname
                   (format "file : ~a, line : ~a, column : ~a -> ~a"
                           fname
                           (parse-error-line   error)
                           (parse-error-column error)
                           (parse-error-msg    error))
                   (format "line : ~a, column : ~a -> ~a"
                           (parse-error-line   error)
                           (parse-error-column error)
                           (parse-error-msg    error))))))

(define/contract (parsed-time-level p)
  (-> parsed? symbol?)
  (let ([token (parsed-token p)])
    (match token
      ['+YEAR    'year]
      ['+MONTH   'month]
      ['+WEEK    'week]
      ['+DAY     'day]
      ;['+DATE    'date]
      ['+TIME    'minute]
      ;['+DURA    'time]
      ['+FROM+TO (let ([unit (list-ref (parsed-strings p) 0)])
                   (match unit
                     ["year"  'year]
                     ["month" 'month]
                     ["week"  'week]
                     ["day"   'day]
                     ["date"  'date]
                     ["time"  'time]))]
      ['+RECUR   (let ([cycle (hash-ref (parsed-attrs p) 'cycle)])
                   (match cycle
                     ["yearly"                         'year]
                     ["monthly"                        'month]
                     ["weekly"                         'week]
                     ["daily"                          'day]
                     ["hourly"                         'time]
                     [(list _ (regexp #rx"year(s)?"))  'year]
                     [(list _ (regexp #rx"month(s)?")) 'month]
                     [(list _ (regexp #rx"week(s)?"))  'week]
                     [(list _ (regexp #rx"day(s)?"))   'day]))]
      [_         'no-time-level])))
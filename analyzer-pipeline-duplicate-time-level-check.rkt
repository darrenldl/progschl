#lang racket

(require "analyzer-components.rkt"
         "analyzer-utils.rkt"
         "parsed-utils.rkt")

(provide duplicate-time-level-check)

(define/contract (duplicate-time-level-check vect)
  (-> (vector/c analysis-ctxt? list?)
      (or/c (vector/c analysis-ctxt? list?) (listof analysis-error?)))
  (define/contract (duplicate-check ctxt level)
    (-> analysis-ctxt? symbol?
        (listof analysis-error?))
    (define (duplicate-check-helper lst [cur 0] [res null])
      (match lst
        [(list)          (reverse res)]
        [(list v vs ...) (cond
                           [(= cur 0) (duplicate-check-helper vs (add1 cur) res)]
                           [else      (duplicate-check-helper vs
                                                              (add1 cur)
                                                              (let ([keyword (hash-ref (parsed-attrs v) 'keyword)])
                                                                (cons (analysis-error (file-name)
                                                                                      (syntax-line   keyword)
                                                                                      (syntax-column keyword)
                                                                                      (format "~a : code block of same time level already exists in same branch"
                                                                                              (syntax->datum keyword)))
                                                                      res)))])]))
    (duplicate-check-helper (hash-ref (analysis-ctxt->hash ctxt) level null)))
  (define errors (let ([ctxt     (vector-ref vect 0)]
                       [sym-list (list 'year-parsed 'month-parsed 'week-parsed' day-parsed)])
                   (foldr append null (map (λ (x) (duplicate-check ctxt x)) sym-list))))
  (cond
    [(= (length errors) 0) vect]
    [else                  errors]))
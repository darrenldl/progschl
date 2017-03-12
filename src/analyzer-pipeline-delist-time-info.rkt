#lang racket

(require "analyzer-utils.rkt"
         "analyzer-components.rkt"
         "parsed-utils.rkt")

(provide delist-time-info)

(define/contract (delist-time-info vect)
  (-> (vector/c analysis-ctxt? (listof parsed?))
      (vector/c analysis-ctxt? (listof parsed?)))
  (define/contract (delist-time-info-helper ctxt-hash [keys (list 'year-info
                                                                  'month-info
                                                                  'week-info
                                                                  'day-info
                                                                  'minute-info)])
    (->* (analysis-ctxt-hash?)
         ((listof symbol?))
         analysis-ctxt-hash?)
    (match keys
      [(list)          ctxt-hash]
      [(list k ks ...) (delist-time-info-helper (hash-set ctxt-hash
                                                          k (let ([lst (hash-ref ctxt-hash k)])
                                                              (delist-single lst)))
                                                ks)]))
  (let* ([ctxt        (vector-ref vect 0)]
         [ctxt-hash   (analysis-ctxt->hash ctxt)]
         [parsed-list (vector-ref vect 1)]
         [res         (hash->analysis-ctxt
                       (delist-time-info-helper ctxt-hash))])
    (vector res parsed-list)))
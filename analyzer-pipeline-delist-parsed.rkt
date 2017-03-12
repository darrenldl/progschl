#lang racket

(require "analyzer-utils.rkt"
         "analyzer-components.rkt"
         "parsed-utils.rkt")

(provide delist-parsed)

(define/contract (delist-parsed vect)
  (-> (vector/c analysis-ctxt? (listof parsed?))
      (vector/c analysis-ctxt? (listof parsed?)))
  (define/contract (delist-parsed-helper ctxt-hash [keys (list 'year-parsed  
                                                               'month-parsed
                                                               'week-parsed
                                                               'day-parsed
                                                               'minute-parsed)])
    (->* (analysis-ctxt-hash?)
         ((listof symbol?))
         analysis-ctxt-hash?)
    (match keys
      [(list)          ctxt-hash]
      [(list k ks ...) (delist-parsed-helper (hash-set ctxt-hash
                                                       k (let ([lst (hash-ref ctxt-hash k)])
                                                           (match lst
                                                             [(list)   #f]
                                                             [(list v) v])))
                                             ks)]))
  (let* ([ctxt        (vector-ref vect 0)]
         [ctxt-hash   (analysis-ctxt->hash ctxt)]
         [parsed-list (vector-ref vect 1)]
         [res         (hash->analysis-ctxt
                       (delist-parsed-helper ctxt-hash))])
    (vector res parsed-list)))
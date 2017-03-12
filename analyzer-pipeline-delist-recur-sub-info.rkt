#lang racket

(require "analyzer-components.rkt"
         "analyzer-utils.rkt"
         "parsed-utils.rkt")

(provide delist-recur-sub-info)

(define/contract (delist-recur-sub-info vect)
  (-> (vector/c analysis-ctxt? (listof parsed?))
      (vector/c analysis-ctxt? (listof parsed?)))
  (define/contract (delist-recur-sub-info-helper ctxt-hash [keys (list 'year
                                                                       'month
                                                                       'day
                                                                       'minute)])
    (->* (analysis-ctxt-hash?)
         (list?)
         analysis-ctxt-hash?)
    (match keys
      [(list)          ctxt-hash]
      [(list k ks ...) (let* ([ctxt-start@-key (string->symbol
                                                (format "~a-recur-start@-info" k))]
                              [ctxt-end@-key   (string->symbol
                                                (format "~a-recur-end@-info"   k))]
                              [start@-list     (hash-ref ctxt-hash ctxt-start@-key)]
                              [end@-list       (hash-ref ctxt-hash ctxt-end@-key)])
                         (delist-recur-sub-info-helper (hash-set* ctxt-hash
                                                                  ctxt-start@-key (delist-single start@-list)
                                                                  ctxt-end@-key   (delist-single end@-list))
                                                       ks))]))
  (let* ([ctxt          (vector-ref vect 0)]
         [parsed-list   (vector-ref vect 1)]
         [ctxt-hash     (analysis-ctxt->hash ctxt)]
         [res-ctxt-hash (delist-recur-sub-info-helper ctxt-hash)]
         [res-ctxt      (hash->analysis-ctxt res-ctxt-hash)])
    (vector res-ctxt parsed-list)))
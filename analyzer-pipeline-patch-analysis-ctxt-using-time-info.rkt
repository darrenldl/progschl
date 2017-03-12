#lang racket

(require "analyzer-utils.rkt"
         "analyzer-components.rkt"
         "parsed-utils.rkt")

(provide patch-analysis-ctxt-using-time-info)

(define/contract (patch-analysis-ctxt-using-time-info vect)
  (-> (vector/c analysis-ctxt? list?)
      (or/c (vector/c analysis-ctxt? list?) (listof analysis-error?)))
  (define/contract (patch-analysis-ctxt-using-time-info-helper ctxt-hash [keys (list 'year 'month 'week 'day)])
    (->* (analysis-ctxt-hash?)
         (list?)
         analysis-ctxt-hash?)
    (match keys
      [(list)          ctxt-hash]
      [(list k ks ...) (let* ([val      (hash-ref ctxt-hash k)]
                              [info-key (string->symbol
                                         (format "~a-info" k))]
                              [info     (hash-ref ctxt-hash info-key)]
                              [info-val (cond
                                          [(empty? info) #f]
                                          [else          (cdr (first info))])])
                         (cond
                           [(and (not val)
                                 info-val) (patch-analysis-ctxt-using-time-info-helper (hash-set ctxt-hash
                                                                                                 k info-val)
                                                                                       ks)]
                           [else           (patch-analysis-ctxt-using-time-info-helper ctxt-hash ks)]))]))
  (let* ([ctxt        (vector-ref vect 0)]
         [ctxt-hash   (analysis-ctxt->hash ctxt)]
         [parsed-list (vector-ref vect 1)]
         [res-hash    (patch-analysis-ctxt-using-time-info-helper ctxt-hash)])
    (vector (hash->analysis-ctxt res-hash)
            parsed-list)))
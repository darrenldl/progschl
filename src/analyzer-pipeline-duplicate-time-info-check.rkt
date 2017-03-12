#lang racket

(require "analyzer-components.rkt"
         "analyzer-utils.rkt"
         "parsed-utils.rkt"
         "macro-utils.rkt"
         "misc-utils.rkt")

(provide duplicate-time-info-check)

(define/contract (duplicate-time-info-check vect)
  (-> (vector/c analysis-ctxt? list?)
      (or/c (vector/c analysis-ctxt? list?) (listof analysis-error?)))
  (define/contract (duplicate-time-info-check-helper ctxt-hash [keys (list 'year-info 'month-info 'week-info 'day-info)] [res null])
    (->* (analysis-ctxt-hash?)
         ((listof symbol?)
          list?)
         (listof analysis-error?))
    (match keys
      [(list)          res]
      [(list k ks ...) (let* ([lst (hash-ref ctxt-hash k null)]
                              [len (length lst)])
                         (cond
                           [(<= len 1) (duplicate-time-info-check-helper ctxt-hash ks res)]
                           [else       (duplicate-time-info-check-helper ctxt-hash
                                                                         ks
                                                                         (append res
                                                                                 (infos->analysis-error-repeated-in-branch (car (first lst))
                                                                                                                           (list-tail lst 1))))]))]))
  (let* ([ctxt      (vector-ref vect 0)]
         [ctxt-hash (analysis-ctxt->hash ctxt)]
         [res       (duplicate-time-info-check-helper ctxt-hash)])
    (cond
      [(empty? res) vect]
      [else         res])))
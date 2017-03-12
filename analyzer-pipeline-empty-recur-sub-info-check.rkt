#lang racket

(require "analyzer-utils.rkt"
         "analyzer-components.rkt"
         "parsed-utils.rkt")

(provide empty-recur-sub-info-check)

(define/contract (empty-recur-sub-info-check vect)
  (-> (vector/c analysis-ctxt? list?)
      (or/c (vector/c analysis-ctxt? list?) (listof analysis-error?)))
  (define/contract (empty-recur-sub-info-check-single p)
    (-> parsed?
        (listof analysis-error?))
    (cond
      [(parsed-with-token p '+RECUR) (let ([start@-info (hash-ref (parsed-attrs p) 'start@-info)]
                                           [end@-info   (hash-ref (parsed-attrs p) 'end@-info)])
                                       (append (check-for-empty-infos start@-info)
                                               (check-for-empty-infos end@-info)))]
      [else                          null]))
  (define/contract (empty-recur-sub-info-check-helper ctxt-hash)
    (-> analysis-ctxt-hash?
        (listof analysis-error?))
    (let* ([start@-list (hash-ref ctxt-hash 'recur-all-start@-infos)]
           [end@-list   (hash-ref ctxt-hash 'recur-all-end@-infos)])
      (append (check-for-empty-infos start@-list)
              (check-for-empty-infos end@-list))))
  (let* ([ctxt      (vector-ref vect 0)]
         [ctxt-hash (analysis-ctxt->hash ctxt)]
         [res       (empty-recur-sub-info-check-helper ctxt-hash)])
    (cond
      [(empty? res) vect]
      [else         res])))
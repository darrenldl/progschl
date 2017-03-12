#lang racket

(require "analyzer-utils.rkt"
         "analyzer-components.rkt"
         "parsed-utils.rkt")

(provide recur-info-strip-start-end-keyword)

(define/contract (strip-info-keyword lst [res null])
  (->* (list?)
       (list?)
       list?)
  (match lst
    [(list)          (reverse res)]
    [(list v vs ...) (let ([stripped (cdr v)])
                       (displayln v)
                       (displayln stripped)
                       (strip-info-keyword vs (cons stripped res)))]))

(define/contract (recur-info-strip-start-end-keyword vect)
  (-> (vector/c analysis-ctxt? (listof parsed?))
      (vector/c analysis-ctxt? (listof parsed?)))
  (define/contract (recur-info-strip-start-end-keyword-helper ctxt-hash)
    (-> analysis-ctxt-hash?
        analysis-ctxt-hash?)
    (let* ([start@-list (hash-ref ctxt-hash 'recur-all-start@-infos)]
           [end@-list   (hash-ref ctxt-hash 'recur-all-end@-infos)]
           [start@-res  (strip-info-keyword start@-list)]
           [end@-res    (strip-info-keyword end@-list)])
      (hash-set* ctxt-hash
                 'recur-all-start@-infos start@-res
                 'recur-all-end@-infos   end@-res)))
  (let* ([ctxt          (vector-ref vect 0)]
         [parsed-list   (vector-ref vect 1)]
         [ctxt-hash     (analysis-ctxt->hash ctxt)]
         [res-ctxt-hash (recur-info-strip-start-end-keyword-helper ctxt-hash)]
         [res-ctxt      (hash->analysis-ctxt res-ctxt-hash)])
    (vector res-ctxt parsed-list)))
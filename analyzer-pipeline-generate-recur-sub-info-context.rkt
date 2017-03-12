#lang racket

(require "analyzer-utils.rkt"
         "analyzer-components.rkt"
         "parsed-utils.rkt")

(provide generate-recur-sub-info-context)

(define/contract (fill-in-context-using-grouped-hts ctxt-hash
                                                    start@-grouped-ht
                                                    end@-grouped-ht
                                                    [keys (append (hash-keys start@-grouped-ht)
                                                                  (hash-keys end@-grouped-ht))])
  (->* (analysis-ctxt-hash? hash? hash?)
       (list?)
       analysis-ctxt-hash?)
  (match keys
    [(list)          ctxt-hash]
    [(list k ks ...) (let ([ctxt-start@-key (string->symbol
                                             (format "~a-recur-start@-info" k))]
                           [ctxt-end@-key   (string->symbol
                                             (format "~a-recur-end@-info"   k))])
                       (fill-in-context-using-grouped-hts (hash-set* ctxt-hash
                                                                     ctxt-start@-key (hash-ref start@-grouped-ht k null)
                                                                     ctxt-end@-key   (hash-ref end@-grouped-ht   k null))
                                                          start@-grouped-ht
                                                          end@-grouped-ht
                                                          ks))]))

(define/contract (generate-recur-sub-info-context vect)
  (-> (vector/c analysis-ctxt? (listof parsed?))
      (vector/c analysis-ctxt? (listof parsed?)))
  (define/contract (generate-recur-sub-info-context-helper ctxt-hash)
    (-> analysis-ctxt-hash?
        analysis-ctxt-hash?)
    (let* ([start@-list       (hash-ref ctxt-hash 'recur-all-start@-infos)]
           [end@-list         (hash-ref ctxt-hash 'recur-all-end@-infos)]
           [start@-grouped-ht (group-by-info-time-level start@-list)]
           [end@-grouped-ht   (group-by-info-time-level end@-list)])
      (fill-in-context-using-grouped-hts ctxt-hash start@-grouped-ht end@-grouped-ht)))
  (let* ([ctxt          (vector-ref vect 0)]
         [parsed-list   (vector-ref vect 1)]
         [ctxt-hash     (analysis-ctxt->hash ctxt)]
         [res-ctxt-hash (generate-recur-sub-info-context-helper ctxt-hash)]
         [res-ctxt      (hash->analysis-ctxt res-ctxt-hash)])
    (vector res-ctxt parsed-list)))
                 
#lang racket

(require "analyzer-utils.rkt"
         "analyzer-components.rkt"
         "misc-utils.rkt"
         "parsed-utils.rkt")

(provide generate-recur-info-context)

(define/contract (accumulate-infos lst [res (vector (hash) (hash))])
  (->* (list?)
       ((vector/c (hash/c exact-nonnegative-integer?
                          (listof (cons/c syntax? any/c)))
                  (hash/c exact-nonnegative-integer?
                          (listof (cons/c syntax? any/c)))))
       (vector/c (hash/c exact-nonnegative-integer?
                          (listof (cons/c syntax? any/c)))
                 (hash/c exact-nonnegative-integer?
                          (listof (cons/c syntax? any/c)))))
  (match lst
    [(list)          res]
    [(list v vs ...) (cond
                       [(parsed-with-token v '+RECUR) (let* ([info-list   (hash-ref (parsed-attrs v) 'info)]
                                                             [grouped-ht  (group-by-info-keyword info-list)]
                                                             [start@-list (hash-ref grouped-ht "start@" null)]
                                                             [end@-list   (hash-ref grouped-ht "end@"   null)])
                                                        (accumulate-infos vs
                                                                          (vector (hash-set (vector-ref res 0)
                                                                                            (parsed-id v) start@-list)
                                                                                  (hash-set (vector-ref res 1)
                                                                                            (parsed-id v) end@-list))))]
                       [else                          (accumulate-infos vs res)])]))

(define/contract (generate-recur-info-context vect)
  (-> (vector/c analysis-ctxt? list?)
      (vector/c analysis-ctxt? list?))
  (define/contract (generate-recur-info-context-helper ctxt-hash lst)
    (-> analysis-ctxt-hash? (listof parsed?)
        analysis-ctxt-hash?)
    (let* ([info-vect        (accumulate-infos lst)]
           [start@-infos-ht  (vector-ref info-vect 0)]
           [end@-infos-ht    (vector-ref info-vect 1)]
           [all-start@-infos (foldl append '() (hash-values start@-infos-ht))]
           [all-end@-infos   (foldl append '() (hash-values end@-infos-ht))])
      (hash-set* ctxt-hash
                 'recur-all-start@-infos        all-start@-infos
                 'recur-all-end@-infos          all-end@-infos
                 'recur-individual-start@-infos start@-infos-ht
                 'recur-individual-end@-infos   end@-infos-ht)))
  (let* ([ctxt          (vector-ref vect 0)]
         [parsed-list   (vector-ref vect 1)]
         [ctxt-hash     (analysis-ctxt->hash ctxt)]
         [res-ctxt-hash (generate-recur-info-context-helper ctxt-hash parsed-list)])
    (vector (hash->analysis-ctxt res-ctxt-hash) parsed-list)))
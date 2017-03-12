#lang racket

(require "analyzer-utils.rkt"
         "analyzer-components.rkt"
         "misc-utils.rkt"
         "parsed-utils.rkt")

(provide generate-add-info-context)

(define/contract (accumulate-infos lst [res null])
  (->* (list?)
       (list?)
       list?)
  (match lst
    [(list)          res]
    [(list v vs ...) (cond
                       [(parsed? v) (let ([info-list (hash-ref (parsed-attrs v) 'info null)])
                                      (accumulate-infos vs
                                                        (append res
                                                                info-list)))]
                       [else        (accumulate-infos vs res)])]))

(define/contract (generate-add-info-context vect)
  (-> (vector/c analysis-ctxt? list?)
      (vector/c analysis-ctxt? list?))
  (let* ([ctxt        (vector-ref vect 0)]
         [parsed-list (vector-ref vect 1)]
         [ctxt-hash   (analysis-ctxt->hash   ctxt)]
         [info-list   (accumulate-infos      parsed-list)]
         [grouped-ht  (group-by-info-keyword info-list)])
    (vector (hash->analysis-ctxt (hash-set* ctxt-hash
                                            'year-info  (hash-ref grouped-ht "+year"  null)
                                            'month-info (hash-ref grouped-ht "+month" null)
                                            'week-info  (hash-ref grouped-ht "+week"  null)
                                            'day-info   (hash-ref grouped-ht "+day"   null)))
            parsed-list)))
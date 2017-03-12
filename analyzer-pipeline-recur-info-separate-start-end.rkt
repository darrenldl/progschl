#lang racket

(require "analyzer-components.rkt"
         "analyzer-utils.rkt"
         "parsed-utils.rkt"
         "macro-utils.rkt"
         "misc-utils.rkt"
         
         "analyzer-pipeline-generate-time-level-context.rkt")

(provide recur-info-separate-start-end)

(define/contract (recur-info-separate-start-end vect)
  (-> (vector/c analysis-ctxt? list?)
      (or/c (vector/c analysis-ctxt? list?) (listof analysis-error?)))
  (define/contract (recur-info-separate-start-end-helper lst [res null])
    (->* (list?)
         (list?)
         list?)
    (match lst
      [(list)          (reverse res)]
      [(list v vs ...) (cond
                         [(parsed-with-token v '+RECUR)
                          (recur-info-separate-start-end-helper vs
                                                                (cons (let* ([info-list   (hash-ref (parsed-attrs v) 'info null)]
                                                                             [grouped-ht  (group-by-info-keyword info-list)]
                                                                             [start@-list (hash-ref grouped-ht "start@" null)]
                                                                             [end@-list   (hash-ref grouped-ht "end@"   null)])
                                                                        (parsed-set-attrs v
                                                                                          'start@-info (cond
                                                                                                         [(empty? start@-list) #f]
                                                                                                         [else                 (cdr (first start@-list))])
                                                                                          'end@-info   (cond
                                                                                                         [(empty? end@-list)   #f]
                                                                                                         [else                 (cdr (first end@-list))])))
                                                                      res))]
                         [else
                          (recur-info-separate-start-end-helper vs (cons v res))])]))
  (let* ([ctxt           (vector-ref vect 0)]
         [ctxt-hash      (analysis-ctxt->hash ctxt)]
         [parsed-list    (vector-ref vect 1)]
         [processed-list (recur-info-separate-start-end-helper parsed-list)])
    (sync-list->ctxt (vector ctxt processed-list))))
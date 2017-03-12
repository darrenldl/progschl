#lang racket

(require "analyzer-utils.rkt"
         "analyzer-components.rkt"
         "parsed-utils.rkt")

(provide empty-info-check)

(define/contract (empty-info-check vect)
  (-> (vector/c analysis-ctxt? list?)
      (or/c (vector/c analysis-ctxt? list?) (listof analysis-error?)))
  (define/contract (empty-info-check-helper lst [res null])
    (->* (list?)
         ((listof analysis-error?))
         (listof analysis-error?))
    (match lst
      [(list)  res]
      [(list v vs ...) (cond
                         [(parsed? v) (let ([info (hash-ref (parsed-attrs v) 'info null)])
                                        (empty-info-check-helper vs
                                                                 (append res
                                                                         (check-for-empty-infos info))))]
                         [else        (empty-info-check-helper vs res)])]))
  (let* ([parsed-list (vector-ref vect 1)]
         [res         (empty-info-check-helper parsed-list)])
    (cond
      [(empty? res) vect]
      [else         res])))
#lang racket

(require "analyzer-components.rkt"
         "analyzer-utils.rkt"
         "parsed-utils.rkt"
         "macro-utils.rkt"
         "misc-utils.rkt")

(provide duplicate-recur-sub-info-check)

;(define/contract (accumulate-infos lst key [res null])
;  (->* (list? symbol?)
;       (list?)
;       list?)
;  (match lst
;    [(list)          res]
;    [(list v vs ...) (cond
;                       [(parsed-with-token v '+RECUR) (let ([info-list (hash-ref (parsed-attrs v) key null)])
;                                                        (accumulate-infos vs
;                                                                          key
;                                                                          (append res
;                                                                                  info-list)))]
;                       [else                          (accumulate-infos vs key res)])]))
;
;(define/contract (accumulate-start@-infos lst)
;  (-> list?
;      list?)
;  (accumulate-infos lst 'start@-info))
;
;(define/contract (accumulate-end@-infos lst)
;  (-> list?
;      list?)
;  (accumulate-infos lst 'end@-info))

(define/contract (duplicate-sub-info-check lst)
  (-> list?
      (listof analysis-error?))
  (let ([len (length lst)])
    (cond
      [(<= len 1) null]
      [else       (infos->analysis-error-repeated-in-sub-info (car (first lst))
                                                              (list-tail lst 1))])))

(define/contract (duplicate-recur-sub-info-check vect)
  (-> (vector/c analysis-ctxt? list?)
      (or/c (vector/c analysis-ctxt? list?) (listof analysis-error?)))
  (define/contract (duplicate-recur-sub-info-check-helper ctxt-hash [keys (list 'year
                                                                                'month
                                                                                'day
                                                                                'minute)] [res null])
    (->* (analysis-ctxt-hash?)
         (list? (listof analysis-error?))
         (listof analysis-error?))
    (match keys
      [(list)          res]
      [(list k ks ...) (let* ([ctxt-start@-key (string->symbol
                                                (format "~a-recur-start@-info" k))]
                              [ctxt-end@-key   (string->symbol
                                                (format "~a-recur-end@-info"   k))]
                              [start@-list     (hash-ref ctxt-hash ctxt-start@-key)]
                              [end@-list       (hash-ref ctxt-hash ctxt-end@-key)])
                         (duplicate-recur-sub-info-check-helper ctxt-hash
                                                                ks
                                                                (append res
                                                                        (duplicate-sub-info-check start@-list)
                                                                        (duplicate-sub-info-check end@-list))))]))
  (let* ([ctxt        (vector-ref vect 0)]
         [ctxt-hash   (analysis-ctxt->hash ctxt)]
         [res         (duplicate-recur-sub-info-check-helper ctxt-hash)])
    (cond
      [(empty? res) vect]
      [else         res])))
#lang racket

(require "analyzer-utils.rkt"
         "analyzer-components.rkt"
         "parsed-utils.rkt")

(provide duplicate-tab-space-check)

(define/contract (accumulate-tab-space branches [res null])
  (->* ((listof list?))
       (list?)
       (or/c list?))
  (match branches
    [(list)              (reverse res)]
    [(list lst lsts ...) (let ([p (first lst)])
                           (cond
                             [(parsed-with-token p 'TAB-SPACE) (accumulate-tab-space lsts (cons p res))]
                             [else                             (accumulate-tab-space lsts res)]))]))

(define/contract (duplicate-tab-space-check branches)
  (-> (listof list?)
      (or/c (listof list?) (listof analysis-error?)))
  
  (let* ([tab-space-list (accumulate-tab-space branches)]
         [tab-space-num  (length tab-space-list)])
    (cond
      [(<= tab-space-num 1) branches]
      [else                 (parseds->analysis-error-repeated tab-space-list)])))
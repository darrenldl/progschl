#lang racket

(require "analyzer-utils.rkt"
         "analyzer-components.rkt"
         "parsed-utils.rkt")

(provide empty-block-head-string-check)

(define/contract (empty-block-head-string-check-single p msg)
  (-> parsed? string?
      (or/c #f analysis-error?))
  (let* ([keyword-syn    (hash-ref (parsed-attrs p) 'keyword)]
         [block-head-str (first (parsed-strings p))]
         [block-head-len (string-length block-head-str)])
    (cond
      [(= block-head-len 0) (analysis-error (file-name)
                                            (syntax-line   keyword-syn)
                                            (syntax-column keyword-syn)
                                            msg)]
      [else                 #f])))

(define/contract (empty-block-head-string-check lst)
  (-> list?
      (or/c list? (listof analysis-error?)))
  (define/contract (empty-block-head-string-check-helper lst [res null])
    (->* (list?)
         (list?)
         (listof analysis-error?))
    (match lst
      [(list)          (reverse res)]
      [(list v vs ...) (cond
                         [(parsed-with-token v
                                             'INCLUDE
                                             '+YEAR
                                             '+MONTH
                                             '+WEEK
                                             '+DAY
                                             '+RECUR
                                             '~TASK
                                             '*REMARK) (let* ([msg (match (parsed-token v)
                                                                     ['INCLUDE "missing file name"]
                                                                     ['+YEAR   "missing year"]
                                                                     ['+MONTH  "missing month"]
                                                                     ['+WEEK   "missing week"]
                                                                     ['+DAY    "missing day"]
                                                                     ['+RECUR  "missing recur parameter"]
                                                                     ['~TASK   "missing task name"]
                                                                     ['*REMARK "missing remark name"])]
                                                              [check-res (empty-block-head-string-check-single v msg)])
                                                         (cond
                                                           [check-res (empty-block-head-string-check-helper vs
                                                                                                            (cons check-res
                                                                                                                  res))]
                                                           [else      (empty-block-head-string-check-helper vs res)]))]
                         [else                         (empty-block-head-string-check-helper vs res)])]))
  (let ([res         (empty-block-head-string-check-helper lst)])
    (cond
      [(empty? res) lst]
      [else         res])))
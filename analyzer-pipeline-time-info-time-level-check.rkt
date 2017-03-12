#lang racket

(require "analyzer-components.rkt"
         "analyzer-utils.rkt"
         "parsed-utils.rkt")

(provide time-info-time-level-check)

(define/contract (time-info-time-level-check vect)
  (-> (vector/c analysis-ctxt? (listof parsed?))
      (or/c (vector/c analysis-ctxt? (listof parsed?)) (listof analysis-error?)))
  (define/contract (time-info-time-level-check-helper ctxt-hash [key-pairs (list (cons 'year-parsed  'year-info)
                                                                                 (cons 'month-parsed 'month-info)
                                                                                 (cons 'week-parsed  'week-info)
                                                                                 (cons 'day-parsed   'day-info))] [res null])
    (->* (analysis-ctxt-hash?)
         ((listof (cons/c symbol? symbol?))
          list?)
         (listof analysis-error?))
    (match key-pairs
      [(list)            (reverse res)]
      [(list kp kps ...) (let* ([parsed-key (car kp)]
                                [info-key   (cdr kp)]
                                [parsed-res (hash-ref ctxt-hash parsed-key)]
                                [info-res   (hash-ref ctxt-hash info-key)])
                           (cond
                             [(and parsed-res info-res)
                              (time-info-time-level-check-helper ctxt-hash
                                                                 kps
                                                                 (cons (let* ([keyword-syn        (car info-res)]
                                                                              [keyword-str        (syntax->datum keyword-syn)]
                                                                              [parsed-keyword-syn (hash-ref (parsed-attrs parsed-res) 'keyword)]
                                                                              [parsed-keyword-str (syntax->datum parsed-keyword-syn)])
                                                                         (analysis-error (file-name)
                                                                                         (syntax-line   keyword-syn)
                                                                                         (syntax-column keyword-syn)
                                                                                         (format "additional info ~a overlaps with time level of ~a at ~a:~a"
                                                                                                 keyword-str
                                                                                                 parsed-keyword-str
                                                                                                 (syntax-line   parsed-keyword-syn)
                                                                                                 (syntax-column parsed-keyword-syn))))
                                                                       res))]
                             [else
                              (time-info-time-level-check-helper ctxt-hash
                                                                 kps
                                                                 res)]))]))
  (let* ([ctxt      (vector-ref vect 0)]
         [ctxt-hash (analysis-ctxt->hash ctxt)]
         [res       (time-info-time-level-check-helper ctxt-hash)])
    (cond
      [(empty? res) vect]
      [else         res])))
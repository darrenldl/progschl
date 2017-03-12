#lang racket

(require "analyzer-utils.rkt"
         "analyzer-components.rkt"
         "parsed-utils.rkt")

(provide empty-from-to-string-check)

(define/contract (empty-from-to-string-check-single p)
  (-> parsed?
      (listof analysis-error?))
  (let* ([keyword-syn    (hash-ref (parsed-attrs p) 'keyword)]
         [unit-str     (first  (parsed-strings p))]
         [from-str     (second (parsed-strings p))]
         [to-str       (third  (parsed-strings p))]
         [unit-str-len (string-length unit-str)]
         [from-str-len (string-length from-str)]
         [to-str-len   (string-length to-str)])
    (append
;     (cond
;       [(= unit-str-len 0) (list (analysis-error (file-name)
;                                           (syntax-line   keyword-syn)
;                                           (syntax-column keyword-syn)
;                                           "missing unit"))]
;       [else               null])
     (cond
       [(= from-str-len 0) (list (analysis-error (file-name)
                                           (syntax-line   keyword-syn)
                                           (syntax-column keyword-syn)
                                           "missing from time"))]
       [else               null])
     (cond
       [(= to-str-len 0)   (list (analysis-error (file-name)
                                           (syntax-line   keyword-syn)
                                           (syntax-column keyword-syn)
                                           "missing to time"))]
       [else               null])
     )
    )
  )

(define/contract (empty-from-to-string-check lst)
  (-> list?
      (or/c list? (listof analysis-error?)))
  (define/contract (empty-from-to-string-check-helper lst [res null])
    (->* (list?)
         (list?)
         (listof analysis-error?))
    (match lst
      [(list)          res]
      [(list v vs ...) (cond
                         [(parsed-with-token v
                                             '+FROM+TO) (let* ([check-res (empty-from-to-string-check-single v)])
                                                         (cond
                                                           [(empty? check-res) (empty-from-to-string-check-helper vs res)]
                                                           [else               (empty-from-to-string-check-helper vs
                                                                                                                  (append res
                                                                                                                          check-res))]))]
                         [else                          (empty-from-to-string-check-helper vs res)])]))
  (let ([res         (empty-from-to-string-check-helper lst)])
    (cond
      [(empty? res) lst]
      [else         res])))
#lang racket

(require "analyzer-utils.rkt"
         "analyzer-components.rkt"
         "misc-utils.rkt"
         "parsed-utils.rkt")

(provide duplicate-recur-info-check)

(define/contract (duplicate-recur-info-check vect)
  (-> (vector/c analysis-ctxt? list?)
      (or/c (vector/c analysis-ctxt? list?) (listof analysis-error?)))
;  (define/contract (duplicate-recur-info-check-single info-ht [keys (hash-keys info-ht)] [res null])
;    (->* (hash?)
;         (list? (listof analysis-error?))
;         (listof analysis-error?))
;    (match keys
;      [(list)          res]
;      [(list k ks ...) (let* ([lst (hash-ref info-ht k)]
;                              [len (length lst)])
;                         (cond
;                           [(<= len 1) (duplicate-recur-info-check-single info-ht ks res)]
;                           [else       (duplicate-recur-info-check-single info-ht ks (append res
;                                                                                        (infos->analysis-error-repeated-in-info (car (first lst))
;                                                                                                                                (rest lst))))]))]))
  (define/contract (duplicate-recur-info-check-helper start@-ht end@-ht [keys (remove-duplicates
                                                                               (append (hash-keys start@-ht)
                                                                                       (hash-keys end@-ht)))] [res null])
    (->* (hash? hash?)
         ((listof exact-nonnegative-integer?) (listof analysis-error?))
         (listof analysis-error?))
    (define/contract (check-if-list-too-long lst)
      (-> list?
          (listof analysis-error?))
      (let ([len (length lst)])
        (cond
          [(<= len 1) null]
          [else       (infos->analysis-error-repeated-in-info (car (first lst))
                                                                          (rest lst))])))
    (match keys
      [(list)          res]
      [(list k ks ...) (let* ([start@-list       (hash-ref start@-ht k null)]
                              [end@-list         (hash-ref end@-ht   k null)]
                              ;[grouped-start@-ht (group-by-info-keyword start@-list)]
                              ;[grouped-end@-ht   (group-by-info-keyword end@-list)]
                              ;[start@-res        (duplicate-recur-info-check-single grouped-start@-ht)]
                              ;[end@-res          (duplicate-recur-info-check-single grouped-end@-ht)]
                              [start@-res        (check-if-list-too-long start@-list)]
                              [end@-res          (check-if-list-too-long end@-list)])
                         (duplicate-recur-info-check-helper start@-ht
                                                            end@-ht
                                                            ks
                                                            (append res start@-res end@-res)))]))
  (let* ([ctxt        (vector-ref vect 0)]
         [parsed-list (vector-ref vect 1)]
         [start@-ht   (analysis-ctxt-recur-individual-start@-infos ctxt)]
         [end@-ht     (analysis-ctxt-recur-individual-end@-infos   ctxt)]
         [res         (duplicate-recur-info-check-helper start@-ht end@-ht)])
    (cond
      [(empty? res) vect]
      [else         res])))
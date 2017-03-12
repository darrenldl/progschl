#lang racket

(require "time-point-struct-def.rkt"
         "contexts-def.rkt"
         "time-point-gen-struct-def.rkt")

(define/contract (min-for-time-level eval-ctxt-hash tl)
  (-> eval-ctxt-hash? symbol?
      integer?)
  (match tl
    ['year         0]
    ['month        1]
    ['week         1]
    ['day          1]
    ['minute       0]))

(define/contract (max-for-time-level eval-ctxt-hash tl)
  (-> eval-ctxt-hash? symbol?
      integer?)
  (match tl
    ['year         3000]
    ['month        12]
    ['week         (weeks-in-month (hash-ref eval-ctxt-hash 'year)
                                   (hash-ref eval-ctxt-hash 'month))]
    ['day          (days-in-month  (hash-ref eval-ctxt-hash 'year)
                                   (hash-ref eval-ctxt-hash 'month))]
    ['minute       (- (* 24 60) 1)]))

(define/contract (infer-for-first/last in eval-ctxt-hash tl)
  (-> (or/c integer? symbol?) eval-ctxt-hash? symbol?
      integer?)
  (match in
    ['min (min-for-time-level eval-ctxt-hash tl)]
    ['max (max-for-time-level eval-ctxt-hash tl)]
    [_    in]))

(define/contract (replace-missing-and-λ-fields-w-0 ht)
  (-> hash?
      hash?)
  (define/contract (replace-missing-and-λ-fields-w-0-helper ht [keys (hash-keys ht)])
    (->* (hash?)
         (list?)
         hash?)
    (match keys
      [(list)          ht]
      [(list k ks ...) (let ([val (hash-ref ht k)])
                         (cond
                           [(or (equal?       val #f)
                                (next-time-λ? val))   (replace-missing-and-λ-fields-w-0-helper (hash-set ht
                                                                                                         k 0)
                                                                                               ks)]
                           [else                      (replace-missing-and-λ-fields-w-0-helper ht ks)]))]))
  (replace-missing-and-λ-fields-w-0-helper ht))
    
(define/contract (generate-eval-ctxt time-gen last-time-point)
  (-> time-point-gen? time-point?
      eval-ctxt?)

  (let* ([time-gen-hash     (time-point-gen->hash time-gen)]
         [last-time-hash    (time-point->hash     last-time-point)]
         [next-time-λ-count (length (filter-hash-val next-time-λ? time-gen-hash))])
    (cond
      [(= next-time-λ-count 0) (let ([ctxt-hash-from-time-point last-time-hash])
                                 (hash->eval-ctxt ctxt-hash-from-time-point))]
      [else                    (let ([ctxt-hash-from-time-gen   time-gen-hash])
                                 (hash->eval-ctxt
                                  (replace-missing-and-λ-fields-w-0 ctxt-hash-from-time-gen)))])))

(define/contract (gen-time-point last-time-point time-point-gen)
  (-> time-point?
      time-point-gen?
      time-point?)
  (define/contract (gen-time-point-helper (tp tpg-hash [ctxt-hash (hash)] ))
    (->* (time-point? time-point-gen-hash?)
         (eval-ctxt-hash?)
         time-point-gen?)
    (
#lang racket

(require "hashable-struct.rkt"
         "parsed-struct-def.rkt"
         "macro-utils.rkt"
         "contexts-def.rkt"
         "date-time-utils.rkt"
         (for-syntax racket/syntax))

(provide ;next-time-λ
         ;next-time-λ?

         analysis-ctxt-dep-λ
         analysis-ctxt-dep-λ?)

(define/contract (list-missing-ctxts ctxt-ht lst)
  (-> hash? list? list?)
  (define (gen-missing-list in [res null])
    (match in
      [(list)          res]
      [(list v vs ...) (cond
                         [(hash-has-key? ctxt-ht v) (gen-missing-list vs res)]
                         [else                      (gen-missing-list vs (cons v res))])]))
  (gen-missing-list lst))

(define/contract (list-incompat-ctxts ctxt-ht lst)
  (-> hash? list? list?)
  (define (gen-incompat-list in [res null])
    (match in
      [(list)          res]
      [(list v vs ...) (cond
                         [(hash-has-key? ctxt-ht v) (gen-incompat-list vs (cons v res))]
                         [else                      (gen-incompat-list vs res)])]))
  (gen-incompat-list lst))

;(define-syntax (next-time-λ stx)
;  (syntax-case stx (ranged wildcard recur custom)
;    [(_ ranged   time-level (first-in last-in))              #'(λ (eval-ctxt-hash)
;                                                                 (let ([val   (hash-ref eval-ctxt-hash time-level)]
;                                                                       [first (infer-for-first/last first-in
;                                                                                                    eval-ctxt-hash
;                                                                                                    time-level)]
;                                                                       [last  (infer-for-first/last last-in
;                                                                                                    eval-ctxt-hash
;                                                                                                    time-level)])
;                                                                   (cond
;                                                                     [(val  . <  . first)                 first]
;                                                                     [(in-range-half-open val first last) (add1 val)]
;                                                                     [(last . <= . val)                   'end])))]
;    [(_ wildcard time-level)                                 #'(λ (eval-ctxt-hash)
;                                                                 (let ([val (hash-ref eval-ctxt-hash time-level)])
;                                                                   (add1 val)))]
;    [(_ recur    time-level (period))                        #'(λ (eval-ctxt-hash)
;                                                                 (let ([val (hash-ref eval-ctxt-hash time-level)])
;                                                                   (+ val period))
;;                                                                 (let* ([val (hash-ref eval-ctxt-hash time-level)]
;;                                                                        [bin (floor (/ (- val start) period))])
;;                                                                   (+ first
;;                                                                      (* period (add1 bin))))
;                                                                 )]
;    [(_ custom   time-level                      (body ...)) #'(next-time-λ custom (eval-ctxt-hash
;                                                                                    time-level
;                                                                                    val)           (body ...))]
;    [(_ custom   time-level (eval-ctxt-hash-name
;                             time-level-name
;                             val-name)           (body ...)) #'(λ (eval-ctxt-hash-name)
;                                                                 (let ([val-name (hash-ref eval-ctxt-hash-name time-level)])
;                                                                   body
;                                                                   ...))]
;    )
;  )

(define analysis-ctxt-dep-λ?
  (-> hash? any/c))

(define-syntax (analysis-ctxt-dep-λ stx)
  (syntax-case stx ()
    [(_ (in-use ...) (not-in-use ...) body ...) #'(λ (analysis-ctxt-hash)
                                                    (let* ([all-reqs-present?     ((listof null) (map (λ (x)
                                                                                                        (hash-ref analysis-ctxt-hash x null))
                                                                                                      (list in-use     ...)))]
                                                           [all-reqs-not-present? ((listof null) (map (λ (x)
                                                                                                        (hash-ref analysis-ctxt-hash x null))
                                                                                                      (list not-in-use ...)))])
                                                      (if (and all-reqs-present? all-reqs-not-present?)
                                                          (begin
                                                            body ...)
                                                          (cond
                                                            [(not all-reqs-present?)
                                                             (vector 'missing-ctxt
                                                                     (list-missing-ctxts  analysis-ctxt-hash (list in-use     ...)))]
                                                            
                                                            [(not all-reqs-not-present?)
                                                             (vector 'incompat-ctxt
                                                                     (list-incompat-ctxts analysis-ctxt-hash (list not-in-use ...)))]))
                                                      ))]))

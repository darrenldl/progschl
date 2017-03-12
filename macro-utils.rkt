#lang racket

(require (for-syntax racket/syntax
                     racket/match
                     racket/contract))

(provide define-cleanup-list
         except-tokens
         (for-syntax format-ids
                     symbols->strings
                     strings->symbols
                     strings-upcase
                     formats)
         in-range-closed
         in-range-half-open
         equal-symbol?
         hash-remove*
         hash-has-all-of-keys?
         hash-has-some-of-keys?
         hash-has-none-of-keys?
         compose1-with-error-detection)

(begin-for-syntax
  (define (prepend str stx)
    (datum->syntax stx
                   (string->symbol
                    (string-append str
                                   (symbol->string (syntax->datum stx))))))
  (define (prepend-cleanup stx)
    (prepend "cleanup-" stx))
  )

(define-syntax (define-cleanup-list stx)
  (syntax-case stx ()
    [(_ tag item ...) #`(define (#,(prepend-cleanup #'tag) lst)
                          (define (remove-ignored lst)
                            (match lst
                              [(list)             (list)]
                              
                              [(list-rest item b) (remove-ignored b)]
                              ...
                              
                              [(list-rest a    b) (cons (remove-ignored a) (remove-ignored b))]
                              [a                  a]))
  
                          (define (delist lst)
                            (match lst
                              [(list)               (list)]  ; preserve empty list
                              [(list (list-rest a)) (delist a)]
                              [a                    a]))
  
                          (delist (remove-ignored lst)))]))

(define-syntax (except-tokens stx)
  (syntax-case stx ()
    [(_ token ...) #'(λ (x)
                       (and (symbol? x)
                            (not (equal? x token))
                            ...))]))

(define-syntax (equal-symbol? stx)
  (syntax-case stx (λ)
    [(_ λ sym ...) #'(λ (x)
                       (and (symbol? x)
                            (or (equal?  x sym)
                                ...)))]
    [(_ x sym ...) #'(and (symbol? x)
                              (or (equal? x sym)
                                  ...))]
    ))

(begin-for-syntax
  (define (format-ids fmt lst [ctxt #f])
    (define (format-ids-helper lst [res null])
      (match lst
        [(list)           (reverse res)]
        [(list-rest v vs) (format-ids-helper vs
                                             (cons (format-id (if ctxt
                                                                  ctxt
                                                                  v)
                                                              fmt
                                                              v)
                                                   res))]))
;    (cond
;      [(syntax? lst) (format-ids-helper (syntax->list lst))]
;      [else          (format-ids-helper lst)])
    (format-ids-helper (syntax->list lst))
    )
  (define (formats fmt lst)
    (map (λ (x) (format fmt x)) lst))
  (define (symbols->strings lst)
    (map symbol->string (map syntax->datum (syntax->list lst))))
  (define (strings->symbols lst)
    (map string->symbol lst))
  (define (strings-upcase lst)
    (map string-upcase lst))
  )

(define-syntax (in-range-closed stx) ; inclusive
  (syntax-case stx ()
    [(_ low up)   #'(λ (x)
                      (and (integer? x)
                           (low . <= . x)
                           (x   . <= . up)))]
    [(_ x low up) #'(and (integer? x)
                         (low . <= . x)
                         (x   . <= . up))]))

(define-syntax (in-range-half-open stx)
  (syntax-case stx ()
    [(_ low up)   #'(λ (x)
                      (and (integer? x)
                           (low . <= . x)
                           (x   . <  . up)))]
    [(_ x low up) #'(and (integer? x)
                         (low . <= . x)
                         (x   . <  . up))]))

(define-syntax (hash-remove* stx)
  (syntax-case stx ()
    [(_ ht key ...) #'(begin
                        ((compose (λ (x) (hash-remove x key)) ...) ht))
                        ]))

(define-syntax (hash-has-all-of-keys? stx)
  (syntax-case stx ()
    [(_ ht key ...) #'(and (hash-has-key? ht key)
                           ...)]))

(define-syntax (hash-has-some-of-keys? stx)
  (syntax-case stx ()
    [(_ ht key ...) #'(or (hash-has-key? ht key)
                          ...)]))

(define-syntax (hash-has-none-of-keys? stx)
  (syntax-case stx ()
    [(_ ht key ...) #'(and (not (hash-has-key? ht key))
                           ...)]))

(define-syntax (if-not-error-then-call stx)
  (syntax-case stx ()
    [(_ error? f) #'(λ (x)
                      (if (error? x)
                          x
                          (f x)))]))

;(define-syntax (if-not-error-then-call-first stx)
;  (syntax-case stx ()
;    [(_ error? f) #'(λ (x)
;                      (displayln (format "initial : ~a" lst))
;                      (if (for/or ([a lst]) (error? a))
;                          lst
;                          (apply f lst)))]))

;(define-syntax (if-not-error-then-call-rest stx)
;  (syntax-case stx ()
;    [(_ error? f) #'(λ lst
;                      (displayln (format "rest in : ~a" lst))
;                      (let ([delisted (match lst
;                                        [(list (list v)) (list v)]
;                                        [v               v])])
;                        (displayln (format "rest delisted : ~a" delisted))
;                        (if (for/or ([a delisted]) (error? a))
;                            delisted
;                            (apply f lst))))]))

;(define-syntax (if-not-error-then-call-last stx)
;  (syntax-case stx ()
;    [(_ error? f) #'(λ lst
;                      (displayln (format "last in : ~a" lst))
;                      (let ([delisted (match lst
;                                        [(list (list v)) (list v)]
;                                        [v               v])])
;                        (displayln (format "last delisted : ~a" delisted))
;                        (if (for/or ([a delisted]) (error? a))
;                            (match delisted
;                              [(list v) v]
;                              [v        v])
;                            (apply f lst))))]))

;(define-syntax (if-not-error-then-call-only stx)
;  (syntax-case stx ()
;    [(_ error? f) #'(λ lst
;                      ;(displayln (format "only in : ~a" lst))
;                      (if (for/or ([a lst]) (error? a))
;                          (match lst
;                            [(list v) v]
;                            [v        v])
;                          (apply f lst)))]))

(define-syntax (compose1-with-error-detection stx)
  (syntax-case stx ()
    [(_ error? (f ...))
     #'(compose (if-not-error-then-call error? f)
                ...)]))

(module+ test
  (define (k v) (vector (add1 (vector-ref v 0))
                        (add1 (vector-ref v 1))))
  (define (k2 x y) (displayln (format "~a ~a" x y)))
  (define test (compose1-with-error-detection (λ (v) (or (string? (vector-ref v 0))
                                                         (string? (vector-ref v 1)))) (k)))
  (displayln (format "final : ~a" (test (vector 1 2))))
  (newline)
  (displayln (format "final : ~a" (test (vector "a" "b"))))

  )
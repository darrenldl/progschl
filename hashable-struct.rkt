#lang racket

(require  "macro-utils.rkt"
          (for-syntax racket/syntax))

(provide define-hashable-struct-for)

(define-syntax (define-hashable-struct-for stx)
  (syntax-case stx ()
    [(define-hashable-struct-for in-name                ([field default field/c] ...) keyword ...)
     (with-syntax ([disabled-val (generate-temporary)])
       #`(define-hashable-struct-for in-name ((quote disabled-val)) ([field default field/c] ...) keyword ...))]
    [(define-hashable-struct-for in-name (disabled-val) ([field default field/c] ...) keyword ...)
     (with-syntax* ([name                     (format-id  #'define-hashable-struct-for "~a"                #'in-name)]
                    [name-w-defaults          (format-id  #'define-hashable-struct-for "~a-w-defaults"     #'in-name)]
                    [(name-field ...)         (format-ids (format "~a-~~a" (syntax->datum #'name))         #'(field ...))]
                    [name->hash               (format-id  #'define-hashable-struct-for "~a->hash"          #'in-name)]
                    [hash->name               (format-id  #'define-hashable-struct-for "hash->~a"          #'in-name)]
                    [name-hash?               (format-id  #'define-hashable-struct-for "~a-hash?"          #'in-name)]
                    [name?                    (format-id  #'define-hashable-struct-for "~a?"               #'in-name)]
                    [fail-res                 (generate-temporary)]
                    [name-disable-fields      (format-id  #'define-hashable-struct-for "~a-disable-fields" #'in-name)])
       #'(begin
           (define fail-res (gensym))
           
           (define-struct/contract name ([field (or/c disabled-val
                                                      field/c)]
                                         ...
                                         ) keyword ...)

           (define/contract (name-w-defaults)
             (-> name?)
             (name default
                   ...))

           (define name-hash?
             (hash/c (or/c (quote field)
                           ...)

                     any/c

                     #:immutable #t
                     #:flat?     #t
                     ))

           (define/contract (name->hash in)
             (-> name? hash?)
             
             (define res (make-hash))
             
             (hash-set! res (quote field) (name-field in))
             ...
             
             (make-immutable-hash (hash->list res)))

           (define/contract (hash->name in)
             (-> name-hash? name?)

             (define res (name default ...))

             (let ([got      (hash-ref in (quote field) fail-res)])
               (if (not (equal? got fail-res))
                   (set! res (struct-copy name res
                                          [field got]))
                   (void)))
             ...

             res)

           (define/contract (name-disable-fields in . sym)
             (-> name? (or/c (quote field) ...) (... ...)
                 name?)
             (define (disable-fields-helper [lst sym] [res (name->hash in)])
               (match lst
                 [(list)                res]
                 [(list k ks (... ...)) (disable-fields-helper ks (hash-set res
                                                                      k disabled-val))]))
             (hash->name (disable-fields-helper)))
           ))]))

(module+ test
  (define-hashable-struct-for test ([i 1 any/c]
                                                [o 2 integer?]) #:transparent)

  (define k (test 0 1))

  (define h (hash 'i 0))

  (test-disable-fields (hash->test h) 'i)

  (test-hash? (test->hash k))

  
  )
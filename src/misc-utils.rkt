#lang racket

(provide list-match
         gen-id
         reverse-lists-in-hash
         filter-hash-val)

(define/contract (list-match compare lst1 lst2 [matched 0])
  (->* ((any/c any/c . -> . boolean?) list? list?)
       (exact-nonnegative-integer?)
       exact-nonnegative-integer?)
  (cond
    [(empty? lst1)                   matched]
    [(empty? lst2)                   matched]
    [(compare (car lst1) (car lst2)) (list-match compare (cdr lst1) (cdr lst2) (add1 matched))]
    [else                            matched]))

(define gen-id
  (let ([id 0])
    (λ ()
      (begin0
        id
        (set! id (add1 id))))))

(define/contract (reverse-lists-in-hash ht)
  (-> hash? hash?)
  (define (reverse-lists-in-hash-helper lst [res ht])
    (match lst
      [(list)          res]
      [(list v vs ...) (reverse-lists-in-hash-helper vs
                                                     (let ([e (hash-ref res v)])
                                                       (cond
                                                         [(list? e) (hash-set res
                                                                              v (reverse e))]
                                                         [else      res])))]))
  (reverse-lists-in-hash-helper (hash-keys ht)))

(define/contract (filter-hash-val pred ht)
  (-> procedure? hash?
      (listof cons?))
  (define/contract (filter-hash-val-helper lst [res null])
    (->* ((listof cons?))
         ((listof cons?))
         (listof cons?))
    (match lst
      [(list)          (reverse res)]
      [(list v vs ...) (let ([val [cdr v]])
                         (cond
                           [(pred val) (filter-hash-val-helper vs (cons v res))]
                           [else       (filter-hash-val-helper vs res)]))]))
  (let ([lst (hash->list ht)])
    (filter-hash-val-helper lst)))
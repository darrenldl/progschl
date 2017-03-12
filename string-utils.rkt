#lang racket

(provide remove-head-tail-space
         remove-head-tail-empty-lines
         string-match
         string->lines
         remove-extra-indent
         regexp-match-list)

(require "misc-utils.rkt")

(define (remove-head-tail-space str)
  (let* ([tail-removed (regexp-replace #rx"[ \t]*$" str "")]
         [head-removed (regexp-replace #rx"^[ \t]*" tail-removed "")])
    head-removed))

(define (remove-head-tail-empty-lines str)
  (let* ([tail-removed (regexp-replace #rx"\n[ \t]*$" str "")]
         [head-removed (regexp-replace #rx"^[ \t]*\n" tail-removed "")])
    head-removed))

(define/contract (string-match first second #:ci? [ci? #f])
  (->* ((or/c string? (listof char?))
        (or/c string? (listof char?)))
       (#:ci? boolean?)
       exact-nonnegative-integer?)
  (let* ([first  (if (string? first)
                     (string->list first)
                     first)]
         [second (if (string? second)
                     (string->list second)
                     second)])
    (if ci?
        (list-match char-ci=? first second)
        (list-match char=?    first second))
    ))

(define/contract (string->lines str)
  (-> string? (listof string?))
  (string-split str #rx"\n"))

(define/contract (remove-extra-indent strings)
  (-> (listof string?) (listof string?))
  (define (remove-indent in trim-len [done null])
    (match in
      [(list)          (reverse done)]
      [(list v vs ...) (remove-indent vs
                                      trim-len
                                      (cons (substring v trim-len)
                                            done))]))
  (define (find-common-indent in [indent #f])
    (match in
      [(list)          indent]
      [(list v vs ...) (find-common-indent vs
                                           (let ([cur-indent (string-length (regexp-replace #rx"^([ \t]*).*" v "\\1"))])
                                             (if indent
                                                 (min indent
                                                      cur-indent)
                                                 cur-indent)))]))
  (remove-indent strings (find-common-indent strings)))

(define/contract (regexp-match-list pattern lst [cur 0] [res null])
  (->* ((or/c regexp? string?)
        (listof string?))
       (exact-nonnegative-integer?
        list?)
       (listof exact-nonnegative-integer?))
  (match lst
    [(list)          res]
    [(list v vs ...) (if (regexp-match pattern v)
                         (regexp-match-list pattern
                                            vs
                                            (add1 cur)
                                            (cons cur res))
                         (regexp-match-list pattern
                                            vs
                                            (add1 cur)
                                            res))]))
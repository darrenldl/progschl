#lang racket

(define-struct/contract amendment ([line     exact-positive-integer?]
                                   [column   exact-nonnegative-integer?]
                                   [position exact-positive-integer?]
                                   [span     exact-nonnegative-integer?]
                                   [type     remove-or-insert?]
                                   [info     (or/c exact-nonnegative-integer?
                                                   string?)]                  #:transparent))

(define (remove-or-insert? x)
  (and (symbol? x)
       (equal? x 'remove)
       (equal? x 'insert)))
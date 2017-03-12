#lang racket

(provide defuzz-error
         defuzz-error?
         defuzz-error-fname
         defuzz-error-line
         defuzz-error-column
         defuzz-error-msg)

(define-struct/contract defuzz-error ([fname  (or/c string? #f)]
                                      [line   exact-positive-integer?]
                                      [column exact-nonnegative-integer?]
                                      [msg    string?])                   #:transparent)
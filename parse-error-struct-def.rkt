#lang racket

(provide parse-error
         parse-error?
         parse-error-fname
         parse-error-line
         parse-error-column
         parse-error-msg)

(define-struct/contract parse-error ([fname  (or/c string? #f)]
                                     [line   exact-positive-integer?]
                                     [column exact-nonnegative-integer?]
                                     [msg    string?])                   #:transparent)
#lang racket

(provide analysis-error
         analysis-error?
         analysis-error-fname
         analysis-error-line
         analysis-error-column
         analysis-error-msg)

(define-struct/contract analysis-error ([fname  (or/c string? #f)]
                                        [line   (or/c exact-positive-integer?    #f)]
                                        [column (or/c exact-nonnegative-integer? #f)]
                                        [msg    string?])                             #:transparent)
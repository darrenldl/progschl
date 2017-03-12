#lang racket

(require "hashable-struct.rkt")

(provide time-point
         time-point-w-defaults
         time-point?
         time-point-hash?
         time-point->hash
         hash->time-point
         ;time-point-second
         time-point-minute
         time-point-day
         time-point-week
         time-point-month
         time-point-year)

(define-hashable-struct-for time-point (;[second 0 exact-nonnegative-integer?]
                                        [minute 0 exact-nonnegative-integer?]
                                        ;[hour   0 exact-nonnegative-integer?]
                                        [day    0 exact-nonnegative-integer?]
                                        [week   0 (or/c exact-nonnegative-integer? #f)]
                                        [month  0 exact-nonnegative-integer?]
                                        [year   0 exact-nonnegative-integer?])          #:transparent)
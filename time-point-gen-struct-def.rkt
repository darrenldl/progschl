#lang racket

(require "hashable-struct.rkt"
         "lambdas-def.rkt"
         "time-point-struct-def.rkt")

(provide time-point-gen
         time-point-gen?
         time-point-gen-hash?
         time-point-gen->hash
         hash->time-point-gen
         ;time-point-gen-second
         time-point-gen-minute
         ;time-point-gen-hour
         time-point-gen-day
         time-point-gen-week
         time-point-gen-month
         time-point-gen-year)

;(define time-point-val?
;  (or/c #f exact-nonnegative-integer? next-time-λ?))

(define init-time-point
  (time-point-w-defaults))

(define-hashable-struct-for single-gen ([value       0 exact-nonnegative-integer?]
                                        [increment   0 exact-nonnegative-integer?]
                                        [lower-bound 0 exact-nonnegative-integer?]
                                        [upper-bound 0 exact-nonnegative-integer?]) #:transparent)

(define-hashable-struct-for time-point-gen ([minute (single-gen-w-defaults) single-gen?]
                                            [day    (single-gen-w-defaults) single-gen?]
                                            [week   (single-gen-w-defaults) single-gen?]
                                            [month  (single-gen-w-defaults) single-gen?]
                                            [year   (single-gen-w-defaults) single-gen?]) #:transparent)

;(define-hashable-struct-for time-point-gen (;[second #f time-point-val?]
;                                            [minute #f time-point-val?]
;                                            ;[hour   #f time-point-val?]
;                                            [day    #f time-point-val?]
;                                            [week   #f time-point-val?]
;                                            [month  #f time-point-val?]
;                                            [year   #f time-point-val?]
;                                            [first  #f (or/c #f time-point?)]
;                                            [last   #f (or/c #f time-point?)]) #:transparent)
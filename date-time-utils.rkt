#lang racket

(provide current-hour-minute
         current-minute-of-day
         current-month
         current-year
;         date->week-of-month
;         date->week-of-year
         current-week-of-month
         current-week-of-year
         current-week-day
         current-day
         current-day-of-year
         days-in-month
         weeks-in-month)

(require racket/date
         (except-in gregor
                    date?)
         "macro-utils.rkt"
         "global-parameters.rkt")

(define/contract (current-minute-of-day)
  (-> integer?)
  (let ([cur-date (date-now)])
    (+ (* (date-hour cur-date) 60)
       (date-minute cur-date))))

(define/contract (current-hour-minute)
  (-> (values exact-nonnegative-integer?
              exact-nonnegative-integer?))
  (let ([cur-date (date-now)])
    (values (date-hour   cur-date)
            (date-minute cur-date))))

(define/contract (current-month)
  (-> (integer-in 1 12))
  (let ([cur-date (date-now)])
    (date-month cur-date)))

(define/contract (current-year)
  (-> exact-nonnegative-integer?)
  (let ([cur-date (date-now)])
    (date-year cur-date)))

(define/contract (weeks-in-month y m)
  (-> exact-integer? (integer-in 1 12)
      (integer-in 1 5))
  (let ([last-day-of-month (make-date 0
                                      0
                                      0
                                      (days-in-month y m)
                                      m
                                      y
                                      0
                                      0
                                      #f
                                      0)])
    (date->week-of-month last-day-of-month)))

(define/contract (date->week-of-month in)
  (-> date?
      (integer-in 1 5))
  (let* ([first-day-of-month (seconds->date
                              (date->seconds
                               (make-date 0
                                          0
                                          0
                                          1
                                          (date-month            in)
                                          (date-year             in)
                                          0
                                          0
                                          (date-dst?             in)
                                          (date-time-zone-offset in))))]
         [first-day-week-day (date-week-day first-day-of-month)]
         [end-of-week        (modulo (sub1 (start-of-week)) 7)]
         [end-of-first-week  (add1 (abs (- end-of-week first-day-week-day)))]
         [day                (date-day in)])
    (if (in-range-closed day 1 end-of-first-week)
        1
        (+ (truncate (/ (- day end-of-first-week) 7)) 1))
    ))

(define/contract (date->week-of-year in)
  (-> date?
      (integer-in 1 53))
  (let* ([first-day-of-year (seconds->date
                             (date->seconds
                              (make-date 0
                                         0
                                         0
                                         1
                                         1
                                         (date-year             in)
                                         0
                                         0
                                         (date-dst?             in)
                                         (date-time-zone-offset in))))]
         [first-day-week-day (date-week-day first-day-of-year)]
         [end-of-week        (modulo (sub1 (start-of-week)) 7)]
         [end-of-first-week  (add1 (abs (- end-of-week first-day-week-day)))]
         [day                (date-day in)])
    (if (in-range-closed day 1 end-of-first-week)
        1
        (let* ([first-day-of-year-secs (date->seconds first-day-of-year)]
               [day-secs               (date->seconds in)]
               [days-in-between        (/ (- day-secs first-day-of-year-secs) (* 24 60 60))])
          (+ (truncate (/ days-in-between 7)) 1))
    )))

(define/contract (current-week-of-month)
  (-> (integer-in 1 5))
  (date->week-of-month (date-now)))

(define/contract (current-week-of-year)
  (-> (integer-in 1 53))
  (date->week-of-year (date-now)))

(define/contract (current-week-day)
  (-> (integer-in 0 6))
  (let ([cur-date (date-now)])
    (date-week-day cur-date)))

(define/contract (current-day)
  (-> (integer-in 1 31))
  (let ([cur-date (date-now)])
    (date-day cur-date)))

(define/contract (current-day-of-year)
  (-> (integer-in 0 365))
  (let ([cur-date (date-now)])
    (date-year-day cur-date)))
#lang racket

(require racket/date)

(provide file-name
         tab-space
         date-now
         start-of-week)

(define file-name     (make-parameter #f))

(define tab-space     (make-parameter 4))

(define date-now      (make-parameter (current-date)
;                                      (let ([cur-date (current-date)])
;                                        (make-date (date-second           cur-date)
;                                                   (date-minute           cur-date)
;                                                   (date-hour             cur-date)
;                                                   (date-day              cur-date)
;                                                   (date-month            cur-date)
;                                                   (date-year             cur-date)
;                                                   (date-week-day         cur-date)
;                                                   (date-year-day         cur-date)
;                                                   (date-dst?             cur-date)
;                                                   (date-time-zone-offset cur-date)))
                                      ))

(define start-of-week (make-parameter 6))
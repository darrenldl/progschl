#lang racket

(require "analyzer-utils.rkt"
         "analyzer-components.rkt"
         "parsed-utils.rkt"
         "date-time-utils.rkt")

(provide time-point-gen-add-fields)

(define/contract (time-point-gen-add-fields vect)
  (-> (vector/c time-point-gen? analysis-ctxt? list?)
      (vector/c time-point-gen? analysis-ctxt? list?))
  (define/contract (time-point-gen-add-fields-helper time-gen-hash ctxt)
    (-> time-point-gen-hash? analysis-ctxt?
        time-point-gen-hash?)
    (let* ([year-ctxt   (analysis-ctxt-year  ctxt)]
           [month-ctxt  (analysis-ctxt-month ctxt)]
           [week-ctxt   (analysis-ctxt-week  ctxt)]
           [day-ctxt    (analysis-ctxt-day   ctxt)]
           [minute-ctxt (analysis-ctxt-minute ctxt)]
           [year-gen    (match year-ctxt
                          [#f                  (current-year)]
                          [(vector 'range s e) (next-time-λ ranged 'year (s e))]
                          [(vector 'recur p)   (next-time-λ recur  'year (p))]
                          [_                   year-ctxt])]
           [month-gen   (match month-ctxt
                          [#f                  (cond
                                                 [(false? year-ctxt)        (current-month)]
                                                 [else                      (next-time-λ ranged 'month  (1 'max))])]
                          [(vector 'range s e) (next-time-λ ranged 'month (s e))]
                          [(vector 'recur p)   (next-time-λ recur  'month (p))]
                          [_                   month-ctxt])]
           [week-gen    (match week-ctxt
                          [#f                  (cond
                                                 [(and (false? year-ctxt)
                                                       (false? month-ctxt)) (current-week-of-month)]
                                                 [else                      (next-time-λ ranged 'week   (1 'max))])]
                          [(vector 'range s e) (next-time-λ ranged 'week (s e))]
                          [(vector 'recur p)   (next-time-λ recur  'week (p))]
                          [_                   week-ctxt])]
           [day-gen     (match day-ctxt
                          [#f                  (cond
                                                 [(and (false? year-ctxt)
                                                       (false? month-ctxt)
                                                       (false? week-ctxt))  (current-day)]
                                                 [(and (false? year-ctxt)
                                                       (false? month-ctxt)) (current-week-day)]
                                                 [else                      (next-time-λ ranged 'day    (1 'max))])]
                          [(vector 'range s e) (next-time-λ ranged 'day (s e))]
                          [(vector 'recur p)   (next-time-λ recur  'day (p))]
                          [_                   day-ctxt])]
           [minute-gen  (match minute-ctxt
                          [#f                  (cond
                                                 [(and (false? year-ctxt)
                                                       (false? month-ctxt)
                                                       (false? day-ctxt))   (current-minute-of-day)]
                                                 [else                      (next-time-λ ranged 'minute (1 'max))])]
                          [(vector 'range s e) (next-time-λ ranged 'minute (s e))]
                          [(vector 'recur p)   (next-time-λ recur  'minute (p))]
                          [_                   minute-ctxt])])
      (hash-set* time-gen-hash
                 'year   year-gen
                 'month  month-gen
                 'week   week-gen
                 'day    day-gen
                 'minute minute-gen)))
  (let* ([time-gen          (vector-ref vect 0)]
         [time-gen-hash     (time-point-gen->hash time-gen)]
         [ctxt              (vector-ref vect 1)]
         [parsed-list       (vector-ref vect 2)]
         [res-time-gen-hash (time-point-gen-add-fields-helper time-gen-hash ctxt)]
         [res-time-gen      (hash->time-point-gen res-time-gen-hash)])
    (vector res-time-gen ctxt parsed-list)))
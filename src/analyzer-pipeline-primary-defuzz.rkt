#lang racket

(require "analyzer-utils.rkt"
         "analyzer-components.rkt"
         "analyzer-conversion.rkt"
         "parsed-utils.rkt")

(provide primary-defuzz)

(define/contract (primary-defuzz lst)
  ; context-free defuzz
  ; or generate ctxt-dep-λs (depends on string->* functions)
  (-> list?
      list?)
  (define (primary-defuzz-single x)
    (cond
      [(parsed-with-token x '+YEAR)    (parsed-set-attrs x
                                                         'time (string->year  (first (parsed-strings x))))]
      [(parsed-with-token x '+MONTH)   (parsed-set-attrs x
                                                         'time (string->month (first (parsed-strings x))))]
      [(parsed-with-token x '+WEEK)    (parsed-set-attrs x
                                                         'time (string->week  (first (parsed-strings x))))]
      [(parsed-with-token x '+DAY)     (parsed-set-attrs x
                                                         'time (string->day   (first (parsed-strings x))))]
      [(parsed-with-token x '+TIME)    (parsed-set-attrs x
                                                         'time (string->time  (first (parsed-strings x))))]
      [(parsed-with-token x '+FROM+TO) (let ([unit (list-ref (parsed-strings x) 0)]
                                             [from (list-ref (parsed-strings x) 1)]
                                             [to   (list-ref (parsed-strings x) 2)])
                                         (match unit
                                           ["year"  (parsed-set-attrs x
                                                                      'from (string->year from)
                                                                      'to   (string->year to))]
                                           ["month" (parsed-set-attrs x
                                                                      'from (string->month from)
                                                                      'to   (string->month to))]
                                           ["week"  (parsed-set-attrs x
                                                                      'from (string->week  from)
                                                                      'to   (string->week  to))]
                                           ["day"   (parsed-set-attrs x
                                                                      'from (string->day   from)
                                                                      'to   (string->day   to))]
                                           ["time"  (parsed-set-attrs x
                                                                      'from (string->time  from)
                                                                      'to   (string->time  to))]))]
      [(parsed-with-token x '+RECUR)   (let* ([cycle           (hash-ref (parsed-attrs x) 'cycle)]
                                              [unit-and-period (match cycle
                                                                 ["yearly"                          (list 'year   1)]
                                                                 ["monthly"                         (list 'month  1)]
                                                                 ["weekly"                          (list 'week   1)]
                                                                 ["daily"                           (list 'day    1)]
                                                                 ["hourly"                          (list 'minute 60)]
                                                                 [(list n (regexp #rx"year(s)?"))   (list 'year   n)]
                                                                 [(list n (regexp #rx"month(s)?"))  (list 'month  n)]
                                                                 [(list n (regexp #rx"week(s)?"))   (list 'week   n)]
                                                                 [(list n (regexp #rx"day(s)?"))    (list 'day    n)]
                                                                 [(list n (regexp #rx"hour(s)?"))   (list 'minute (* n 60))]
                                                                 [(list n (regexp #rx"minute(s)?")) (list 'minute n)])])
                                         (match unit-and-period
                                           [(list unit period) (parsed-set-attrs x
                                                                                 'unit   unit
                                                                                 'period period)]))]
      [else                            x]))
  (define/contract (primary-defuzz-helper lst [res null])
    (->* (list?)
         (list?)
         list?)
    (match lst
      [(list)          (reverse res)]
      [(list v vs ...) (primary-defuzz-helper vs
                                              (cons (primary-defuzz-single v) res))]))
  (primary-defuzz-helper lst))
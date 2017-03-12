#lang racket

(require ;"analyzer-utils.rkt"
         "analyzer-components.rkt"
         "parser-components.rkt"
         ;"parsed-utils.rkt"
         "string-utils.rkt"
         "macro-utils.rkt"
         "date-time-utils.rkt")

(provide string->year
         string->month
         string->week
         string->day
         string->time)

(define list-of-months (list "january"
                             "february"
                             "march"
                             "april"
                             "may"
                             "june"
                             "july"
                             "august"
                             "september"
                             "october"
                             "november"
                             "december"))

(define list-of-week-days (list "monday"
                                "tuesday"
                                "wednesday"
                                "thursday"
                                "friday"
                                "saturday"
                                "sunday"))

(define (convert-failure? x)
  (equal-symbol? x
                 'no-match
                 'ambiguous))

(define/contract (string->time str)
  (-> string?
      (or/c convert-failure? (analysis-ctxt-free-vect/c (or/c (integer-in 0 (- (* 24 60) 1)) 'wildcard)) analysis-ctxt-dep-vect/c))
  (let ([res (parse-hour-minute str)])
    (match res
      [(cons hour minute)  (analysis-ctxt-free-vect (+ (* hour 60) minute))]
      [_                   'no-match])))

(define/contract (string->day str)
  (-> string?
      (or/c convert-failure? (analysis-ctxt-free-vect/c (or/c (integer-in 1 32) 'wildcard)) analysis-ctxt-dep-vect/c))
  (let* ([week-days       list-of-week-days]
         [lower-str       (string-downcase str)]
         [res             (string->number str)]
         [regex-res       (regexp-match-list lower-str week-days)]
         [regex-res-count (length regex-res)])
    (cond
      [(string=? str "_")         (analysis-ctxt-free-vect 'wildcard)]
      [(in-range-closed res 1 31) (analysis-ctxt-free-vect res)]
      [(= regex-res-count 0)      (match lower-str
                                    ["today"    (analysis-ctxt-dep-vect (analysis-ctxt-dep-λ () ('year-parsed 'month-parsed 'week-parsed)
                                                                                             (current-day)))]
                                    ["tomorrow" (analysis-ctxt-dep-vect (analysis-ctxt-dep-λ () ('year-parsed 'month-parsed 'week-parsed)
                                                                                             (add1 (current-day))))]
                                    [_          'no-match])]
      [(= regex-res-count 1)      (analysis-ctxt-dep-vect (analysis-ctxt-dep-λ ('week-parsed) ()
                                                                               (first regex-res)))]
      [(> regex-res-count 1)      'ambiguous])))

(define/contract (string->week str)
  (-> string?
      (or/c convert-failure? (analysis-ctxt-free-vect/c (or/c (integer-in 1 6) 'wildcard)) analysis-ctxt-dep-vect/c))
  (let ([res (string->number str)])
    (cond
      [(string=? str "_")        (analysis-ctxt-free-vect 'wildcard)]
      [(in-range-closed res 1 5) (analysis-ctxt-free-vect res)]
      [(integer? res)            'no-match]
      [else                      (match (string-downcase str)
                                   ["this"  (analysis-ctxt-dep-vect (analysis-ctxt-dep-λ () ('year-parsed 'month-parsed)
                                                                                         (current-week-of-month))
                                                                    (analysis-ctxt-dep-λ () ('year-parsed 'month-parsed)
                                                                                         (current-week-of-year)))]
                                   ["next"  (analysis-ctxt-dep-vect (analysis-ctxt-dep-λ () ('year-parsed 'month-parsed)
                                                                                         (add1 (current-week-of-month)))
                                                                    (analysis-ctxt-dep-λ () ('year-parsed 'month-parsed)
                                                                                         (add1 (current-week-of-year))))])])))

(define/contract (string->month str)
  (-> string?
      (or/c convert-failure? (analysis-ctxt-free-vect/c (or/c (integer-in 1 13) 'wildcard)) analysis-ctxt-dep-vect/c))
  (let* ([months          list-of-months]
         [lower-str       (string-downcase str)]
         [res             (string->number str)]
         [regex-res       (regexp-match-list lower-str months)]
         [regex-res-count (length regex-res)])
    (cond
      [(string=? str "_")         (analysis-ctxt-free-vect 'wildcard)]
      [(in-range-closed res 1 12) (analysis-ctxt-free-vect res)]
      [(= regex-res-count 0)      (match lower-str
                                    ["this" (analysis-ctxt-dep-vect (analysis-ctxt-dep-λ () ('year-parsed)
                                                                                         (current-month)))]
                                    ["next" (analysis-ctxt-dep-vect (analysis-ctxt-dep-λ () ('year-parsed)
                                                                                         (add1 (current-month))))]
                                    [_      'no-match])]
      [(= regex-res-count 1)      (analysis-ctxt-free-vect (add1 (first regex-res)))]
      [(> regex-res-count 1)      'ambiguous])))

(define/contract (string->year str)
  (-> string?
      (or/c convert-failure? (analysis-ctxt-free-vect/c (or/c integer? 'wildcard)) analysis-ctxt-dep-vect/c))
  (let ([res (string->number str)])
    (cond
      [(string=? str "_") (analysis-ctxt-free-vect 'wildcard)]
      [res                (analysis-ctxt-free-vect res)]
      [else               (match (string-downcase str)
                            ["this" (analysis-ctxt-dep-vect (analysis-ctxt-dep-λ () ()
                                                                                 (current-year)))]
                            ["next" (analysis-ctxt-dep-vect (analysis-ctxt-dep-λ () ()
                                                                                 (add1 (current-year))))]
                            [_      'no-match])])))
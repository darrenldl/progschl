#lang racket

(require "analyzer-utils.rkt"
         "analyzer-components.rkt"
         "parsed-utils.rkt"
         "date-time-utils.rkt"
         "time-point-struct-def.rkt")

(provide time-point-gen-add-start-end-for-recur)

(define/contract (accumulate-infos lst [res (vector null null)])
  (->* (list?)
       ((vector/c list? list?))
       (vector/c list? list?))
  (match lst
    [(list)          res]
    [(list v vs ...) (cond
                       [(parsed-with-token v '+RECUR) (let ([start@-info (hash-ref (parsed-attrs v) 'start@-info null)]
                                                            [end@-info   (hash-ref (parsed-attrs v) 'end@-info   null)])
                                                        (accumulate-infos vs
                                                                          (vector (append (vector-ref res 0)
                                                                                          start@-info)
                                                                                  (append (vector-ref res 1)
                                                                                          end@-info))))]
                       [else                          (accumulate-infos vs res)])]))

(define/contract (make-first-time-point grouped-info)
  (-> hash?
      time-point?)
  (let ([year   (let ([res (hash-ref grouped-info 'year  null)])
                  (cond
                    [(empty? res) 1]
                    [else         (cdr (first res))]))]
        [month  (let ([res (hash-ref grouped-info 'month null)])
                  (cond
                    [(empty? res) 1]
                    [else         (cdr (first res))]))]
        [week   (let ([res (hash-ref grouped-info 'week  null)])
                  (cond
                    [(empty? res) 1]
                    [else         (cdr (first res))]))]
        [day    (let ([res (hash-ref grouped-info 'day   null)])
                  (cond
                    [(empty? res) 1]
                    [else         (cdr (first res))]))]
        [minute 0])
    (time-point minute
                day
                week
                month
                year)))

(define/contract (make-last-time-point grouped-info)
  (-> hash?
      time-point?)
  (let* ([year-res   (hash-ref grouped-info 'year   null)]
         [month-res  (hash-ref grouped-info 'month  null)]
         [week-res   (hash-ref grouped-info 'week   null)]
         [day-res    (hash-ref grouped-info 'day    null)]
         [minute-res (hash-ref grouped-info 'minute null)]
         [year   (cond
                   [(empty? year-res)  (current-year)]
                   [else               (cdr (first year-res))])]
         [month  (cond
                   [(empty? month-res) (cond
                                         [(empty? year-res) (current-month)]
                                         [else              12])]
                   [else               (cdr (first month-res))])]
         [week   (cond
                   [(empty? week-res)  (cond
                                         [(and (empty? year-res)
                                               (empty? month-res)) (current-week-of-month)]
                                         [else                     (weeks-in-month year month)])]
                   [else               (cdr (first week-res))])]
         [day    (cond
                   [(empty? day-res)   (cond
                                         [(and (empty? year-res)
                                               (empty? month-res)
                                               (empty? week-res))  (current-day)]
                                         [(and (empty? year-res)
                                               (empty? month-res)) (current-week-day)]
                                         [else                     (days-in-month  year month)])]
                   [else               (cdr (first day-res))])]
         [minute (cond
                   [(empty? minute-res)   (cond
                                            [(and (empty? year-res)
                                                  (empty? month-res)
                                                  (empty? week-res))  (current-day)]
                                            [(and (empty? year-res)
                                                  (empty? month-res)) (current-week-day)]
                                            [else                     (- (* 24 60) 1)])]
                   [else               (cdr (first day-res))])])
    (time-point minute
                day
                week
                month
                year)))

(define/contract (time-point-gen-add-start-end-for-recur vect)
  (-> (vector/c time-point-gen? analysis-ctxt? list?)
      (vector/c time-point-gen? analysis-ctxt? list?))
  (define/contract (time-point-gen-add-start-end-for-recur-helper time-gen-hash lst)
    (-> time-point-gen-hash? list?
        time-point-gen-hash?)
    (let* ([infos            (accumulate-infos lst)]
           [start@-info      (vector-ref infos 0)]
           [end@-info        (vector-ref infos 1)]
           [grouped-start@   (group-by-info-time-level start@-info)]
           [grouped-end@     (group-by-info-time-level end@-info)]
           [first-time-point (cond
                               [(empty? start@-info) #f]
                               [else                 (make-first-time-point grouped-start@)])]
           [last-time-point  (cond
                               [(empty? end@-info)   #f]
                               [else                 (make-last-time-point  grouped-end@)])])
      (hash-set* time-gen-hash
                 'first first-time-point
                 'last  last-time-point)))
  (let* ([time-gen          (vector-ref vect 0)]
         [ctxt              (vector-ref vect 1)]
         [parsed-list       (vector-ref vect 2)]
         [time-gen-hash     (time-point-gen->hash time-gen)]
         [res-time-gen-hash (time-point-gen-add-start-end-for-recur-helper time-gen-hash parsed-list)]
         [res-time-gen      (hash->time-point-gen res-time-gen-hash)])
    (vector res-time-gen
            ctxt
            parsed-list)))
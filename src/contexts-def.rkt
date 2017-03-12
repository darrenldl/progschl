#lang racket

(require "hashable-struct.rkt"
         "parsed-utils.rkt")

(provide analysis-ctxt
         analysis-ctxt-w-defaults
         analysis-ctxt?
         analysis-ctxt-hash?
         analysis-ctxt->hash
         hash->analysis-ctxt
         analysis-ctxt-year
         analysis-ctxt-month
         analysis-ctxt-week
         analysis-ctxt-day
         analysis-ctxt-minute
         analysis-ctxt-year-parsed
         analysis-ctxt-month-parsed
         analysis-ctxt-week-parsed
         analysis-ctxt-day-parsed
         analysis-ctxt-minute-parsed
         analysis-ctxt-year-info
         analysis-ctxt-month-info
         analysis-ctxt-week-info
         analysis-ctxt-day-info
         analysis-ctxt-minute-info
         analysis-ctxt-recur-all-start@-infos
         analysis-ctxt-recur-all-end@-infos
         analysis-ctxt-recur-individual-start@-infos
         analysis-ctxt-recur-individual-end@-infos
         analysis-ctxt-year-recur-start@-info
         analysis-ctxt-month-recur-start@-info
         analysis-ctxt-day-recur-start@-info
         analysis-ctxt-minute-recur-start@-info
         analysis-ctxt-year-recur-end@-info
         analysis-ctxt-month-recur-end@-info
         analysis-ctxt-day-recur-end@-info
         analysis-ctxt-minute-recur-end@-info
         analysis-ctxt-disable-fields

         single-val
         single-val-w-defaults
         single-val?

         ranged-val
         ranged-val-w-defaults
         ranged-val?

         recur-val
         recur-val-w-defaults
         recur-val?

;         eval-ctxt
;         eval-ctxt-w-defaults
;         eval-ctxt?
;         eval-ctxt-hash?
;         eval-ctxt->hash
;         hash->eval-ctxt
;         eval-ctxt-second
;         eval-ctxt-minute
;         eval-ctxt-hour
;         eval-ctxt-day
;         eval-ctxt-week
;         eval-ctxt-month
;         eval-ctxt-year
         )

;(define-hashable-struct-for eval-ctxt ([second #f (or/c #f -1 exact-nonnegative-integer?)]
;                                       [minute #f (or/c #f -1 exact-nonnegative-integer?)]
;                                       [hour   #f (or/c #f -1 exact-nonnegative-integer?)]
;                                       [day    -1 (or/c #f -1 exact-nonnegative-integer?)]
;                                       [week   -1 (or/c #f -1 exact-nonnegative-integer?)]
;                                       [month  -1 (or/c #f -1 exact-nonnegative-integer?)]
;                                       [year   #f (or/c #f -1 exact-nonnegative-integer?)]) #:transparent)

(define-hashable-struct-for single-val ([value  0    exact-nonnegative-integer?])             #:transparent)

(define-hashable-struct-for ranged-val ([start  'min (or/c 'min exact-nonnegative-integer?)]
                                        [end    'max (or/c 'max exact-nonnegative-integer?)]) #:transparent)

(define-hashable-struct-for recur-val  ([period 0    exact-nonnegative-integer?])             #:transparent)

(define analysis-ctxt-val/c (or/c #f
                                  single-val?
                                  ranged-val?
                                  recur-val?))

(define-hashable-struct-for analysis-ctxt ('disabled) ([year                          #f     analysis-ctxt-val/c]
                                                       [month                         #f     analysis-ctxt-val/c]
                                                       [week                          #f     analysis-ctxt-val/c]
                                                       [day                           #f     analysis-ctxt-val/c]
                                                       [minute                        #f     analysis-ctxt-val/c]
                                                       [year-parsed                   null   (or/c #f
                                                                                                   parsed?
                                                                                                   (listof parsed?))]
                                                       [month-parsed                  null   (or/c #f
                                                                                                   parsed?
                                                                                                   (listof parsed?))]
                                                       [week-parsed                   null   (or/c #f
                                                                                                   parsed?
                                                                                                   (listof parsed?))]
                                                       [day-parsed                    null   (or/c #f
                                                                                                   parsed?
                                                                                                   (listof parsed?))]
                                                       [minute-parsed                 null   (or/c #f
                                                                                                   parsed?
                                                                                                   (listof parsed?))]
                                                       [year-info                     null   (or/c #f
                                                                                                   (cons/c syntax? any/c)
                                                                                                   (listof (cons/c syntax? any/c)))]
                                                       [month-info                    null   (or/c #f
                                                                                                   (cons/c syntax? any/c)
                                                                                                   (listof (cons/c syntax? any/c)))]
                                                       [week-info                     null   (or/c #f
                                                                                                   (cons/c syntax? any/c)
                                                                                                   (listof (cons/c syntax? any/c)))]
                                                       [day-info                      null   (or/c #f
                                                                                                   (cons/c syntax? any/c)
                                                                                                   (listof (cons/c syntax? any/c)))]
                                                       [minute-info                   null   (or/c #f
                                                                                                   (cons/c syntax? any/c)
                                                                                                   (listof (cons/c syntax? any/c)))]
                                                       [recur-all-start@-infos        null   (or/c 'disabled
                                                                                                   (listof (cons/c syntax? any/c)))]
                                                       [recur-all-end@-infos          null   (or/c 'disabled
                                                                                                   (listof (cons/c syntax? any/c)))]
                                                       [recur-individual-start@-infos (hash) (hash/c exact-nonnegative-integer?
                                                                                                     (or/c #f
                                                                                                           (cons/c syntax? any/c)
                                                                                                           (listof (cons/c syntax? any/c))))]
                                                       [recur-individual-end@-infos   (hash) (hash/c exact-nonnegative-integer?
                                                                                                     (or/c #f
                                                                                                           (cons/c syntax? any/c)
                                                                                                           (listof (cons/c syntax? any/c))))]
                                                       [year-recur-start@-info        null   (or/c #f
                                                                                                   (cons/c syntax? any/c)
                                                                                                   (listof (cons/c syntax? any/c)))]
                                                       [month-recur-start@-info       null   (or/c #f
                                                                                                   (cons/c syntax? any/c)
                                                                                                   (listof (cons/c syntax? any/c)))]
                                                       [day-recur-start@-info         null   (or/c #f
                                                                                                   (cons/c syntax? any/c)
                                                                                                   (listof (cons/c syntax? any/c)))]
                                                       [minute-recur-start@-info      null   (or/c #f
                                                                                                   (cons/c syntax? any/c)
                                                                                                   (listof (cons/c syntax? any/c)))]
                                                       [year-recur-end@-info          null   (or/c #f
                                                                                                   (cons/c syntax? any/c)
                                                                                                   (listof (cons/c syntax? any/c)))]
                                                       [month-recur-end@-info         null   (or/c #f
                                                                                                   (cons/c syntax? any/c)
                                                                                                   (listof (cons/c syntax? any/c)))]
                                                       [day-recur-end@-info           null   (or/c #f
                                                                                                   (cons/c syntax? any/c)
                                                                                                   (listof (cons/c syntax? any/c)))]
                                                       [minute-recur-end@-info        null   (or/c #f
                                                                                                   (cons/c syntax? any/c)
                                                                                                   (listof (cons/c syntax? any/c)))]
                                                       ) #:transparent)
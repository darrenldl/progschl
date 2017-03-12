#lang racket

(require "parsed-utils.rkt"
         "time-point-struct-def.rkt"
         "hashable-struct.rkt"
         "contexts-def.rkt"
         "lambdas-def.rkt"
         "analysis-error-struct-def.rkt"
         "global-parameters.rkt"
         "misc-utils.rkt"
         "analyzer-conversion.rkt")

(provide ambiguous->error-list
         no-match->error-list

         show-analysis-error
         show-analysis-errors

         group-by-info-keyword
         infos->analysis-error-repeated-in-branch
         infos->analysis-error-repeated-in-info
         infos->analysis-error-repeated-in-sub-info

         group-by-info-time-level
         infos->analysis-error-time-level-overlapped

         parseds->analysis-error-repeated

         key->readable-token-name

         delist-single

         apply-analysis-ctxt-dep-λs

         sync-list->ctxt
         sync-ctxt->list
         ;sync-infos->ctxt

         info-time-level
         info-time-level-by-keyword
         sub-info-time-level
         sub-info-time-level-by-keyword

         time-level?
         time-level=?
         time-level<?
         time-level<=?
         time-level>?
         time-level>=?

         check-for-empty-infos

         convert-info
         convert-sub-info)

(require "macro-utils.rkt")

(define/contract (delist-single lst)
  (-> list?
      any/c)
  (match lst
    [(list)   #f]
    [(list v) v]))

(define (key->readable-token-name key)
  (match key
    ['year-parsed  "+year"]
    ['month-parsed "+month"]
    ['week-parsed  "+week"]
    ['day-parsed   "+day"]
    [_             key]))

(define/contract (show-analysis-error error)
  (-> analysis-error?
      void?)
  (displayln (let ([fname (analysis-error-fname error)])
               (if fname
                   (format "file : ~a, line : ~a, column : ~a -> ~a"
                           fname
                           (analysis-error-line   error)
                           (+ (analysis-error-column error) 1)
                           (analysis-error-msg    error))
                   (format "line : ~a, column : ~a -> ~a"
                           (analysis-error-line   error)
                           (+ (analysis-error-column error) 1)
                           (analysis-error-msg    error))))))

(define/contract (show-analysis-errors errors)
  (-> (listof analysis-error?)
      void?)
  (for ([e errors])
    (show-analysis-error e)))

(define/contract (missing-ctxts->errors keyword lst)
  (-> syntax? (listof symbol?) (listof analysis-error?))
  (define (missing-ctxts->errors-helper lst [res null])
    (match lst
      [(list)          (reverse res)]
      [(list v vs ...) (missing-ctxts->errors-helper vs
                                                     (cons (analysis-error (file-name)
                                                                           #f
                                                                           #f
                                                                           (format "missing required context : ~a, for ~a at ~a:~a"
                                                                                   (key->readable-token-name v)
                                                                                   (syntax->datum keyword)
                                                                                   (syntax-line   keyword)
                                                                                   (syntax-column keyword)))
                                                           res))]))
  (missing-ctxts->errors-helper lst))

(define/contract (incompat-ctxts->errors ctxt-hash keyword lst)
  (-> analysis-ctxt-hash? syntax? (listof symbol?) (listof analysis-error?))
  (define (incompat-ctxts->errors-helper lst [res null])
    (match lst
      [(list)          (reverse res)]
      [(list v vs ...) (incompat-ctxts->errors-helper vs
                                                      (cons (let ([src (match (hash-ref ctxt-hash v #f)
                                                                         [(list v) v]
                                                                         [#f       #f])])
                                                              (cond
                                                                [(parsed? src) (let ([src-keyword (hash-ref (parsed-attrs src) 'keyword)])
                                                                                 (analysis-error (file-name)
                                                                                                 (syntax-line   src-keyword)
                                                                                                 (syntax-column src-keyword)
                                                                                                 (format "incompatible context : ~a, for ~a at ~a:~a"
                                                                                                         (key->readable-token-name v)
                                                                                                         (syntax->datum keyword)
                                                                                                         (syntax-line   keyword)
                                                                                                         (syntax-column keyword))))]
                                                                [else          (analysis-error (file-name)
                                                                                               #f
                                                                                               #f
                                                                                               (format "incompatible context : ~a, for ~a at ~a:~a"
                                                                                                       (key->readable-token-name v)
                                                                                                       (syntax->datum keyword)
                                                                                                       (syntax-line   keyword)
                                                                                                       (syntax-column keyword)))]))
                                                            res))]))
  (incompat-ctxts->errors-helper lst))

(define/contract (ambiguous->error stx)
  (-> syntax? analysis-error?)
  (analysis-error (file-name)
                  (syntax-line   stx)
                  (syntax-column stx)
                  (format "\"~a\" is ambiguous" (syntax->datum stx))))

(define/contract (ambiguous->error-list stx)
  (-> syntax? (listof analysis-error?))
  (list (ambiguous->error stx)))

(define/contract (no-match->error stx)
  (-> syntax? analysis-error?)
  (analysis-error (file-name)
                  (syntax-line   stx)
                  (syntax-column stx)
                  (format "\"~a\" has no match" (syntax->datum stx))))

(define/contract (no-match->error-list stx)
  (-> syntax? (listof analysis-error?))
  (list (no-match->error stx)))

(define/contract (apply-analysis-ctxt-dep-λs ctxt-hash keyword lst)
  (-> analysis-ctxt-hash? syntax? (listof analysis-ctxt-dep-λ?)
      any/c)
  (define (apply-analysis-ctxt-dep-λs-helper lst [last-error #f])
    (match lst
      [(list)          last-error]
      [(list f fs ...) (match (f ctxt-hash)
                         [(vector 'missing-ctxt  lst) (apply-analysis-ctxt-dep-λs-helper fs (missing-ctxts->errors            keyword lst))]
                         [(vector 'incompat-ctxt lst) (apply-analysis-ctxt-dep-λs-helper fs (incompat-ctxts->errors ctxt-hash keyword lst))]
                         [v                           v])]))
  (apply-analysis-ctxt-dep-λs-helper lst))

(define/contract (group-by-info-keyword lst)
  (-> list? hash?)
  (define/contract (group-by-info-keyword-helper lst [res (hash)])
    (->* (list?)
         (hash?)
         hash?)
    (match lst
      [(list)          (reverse-lists-in-hash res)]
      [(list v vs ...) (let* ([keyword-syn (car v)]
                              [keyword-str (syntax->datum keyword-syn)])
                         (group-by-info-keyword-helper vs
                                                       (hash-set res
                                                                 keyword-str (cons v
                                                                                   (hash-ref res keyword-str null)))))]))
  (group-by-info-keyword-helper lst))

(define/contract (group-by-info-time-level lst)
  (-> list? hash?)
  (define/contract (group-by-info-time-level-helper lst [res (hash)])
    (->* (list?)
         (hash?)
         hash?)
    (match lst
      [(list)          (reverse-lists-in-hash res)]
      [(list v vs ...) (let* ([keyword-syn (car v)]
                              [keyword-str (syntax->datum keyword-syn)]
                              [time-level  (let ([str-len (string-length keyword-str)])
                                             (cond
                                               [(= str-len 2) (sub-info-time-level-by-keyword keyword-str)]
                                               [else          (info-time-level-by-keyword     keyword-str)]))])
                         (group-by-info-time-level-helper vs
                                                          (hash-set res
                                                                    time-level (cons v
                                                                                     (hash-ref res time-level null)))))]))
  (group-by-info-time-level-helper lst))

(define/contract (infos->analysis-error-repeated first lst loc-desc)
  (-> syntax? list? string?
      list?)
  (map (λ (x)
         (let* ([keyword-syn (car x)]
                [keyword-str (syntax->datum keyword-syn)])
           (analysis-error (file-name)
                           (syntax-line   keyword-syn)
                           (syntax-column keyword-syn)
                           (format "additional info : ~a has already appeared at ~a:~a ~a"
                                   keyword-str
                                   (syntax-line   first)
                                   (syntax-column first)
                                   loc-desc))))
       lst))

(define/contract (infos->analysis-error-time-level-overlapped first lst)
  (-> syntax? list?
      list?)
  (map (λ (x)
         (let* ([keyword-syn (car x)]
                [keyword-str (syntax->datum keyword-syn)])
           (analysis-error (file-name)
                           (syntax-line   keyword-syn)
                           (syntax-column keyword-syn)
                           (format "additional info : ~a overlaps with time level of info ~a at ~a:~a"
                                   keyword-str
                                   (syntax->datum first)
                                   (syntax-line   first)
                                   (syntax-column first)))))
       lst))

(define/contract (infos->analysis-error-repeated-in-branch first lst)
  (-> syntax? list? list?)
  (infos->analysis-error-repeated first lst "in the branch"))

(define/contract (infos->analysis-error-repeated-in-info first lst)
  (-> syntax? list? list?)
  (infos->analysis-error-repeated first lst "in the info list"))

(define/contract (infos->analysis-error-repeated-in-sub-info first lst)
  (-> syntax? list? list?)
  (infos->analysis-error-repeated first lst "in the sub-info list"))

(define/contract (parseds->analysis-error-repeated lst)
  (-> (listof parsed?)
      (listof analysis-error?))
  (define/contract (parseds->analysis-error-repeated-helper first-one-syn lst [res null])
    (->* (syntax? (listof parsed?))
         ((listof analysis-error?))
         (listof analysis-error?))
    (match lst
      [(list)          (reverse res)]
      [(list v vs ...) (parseds->analysis-error-repeated-helper first-one-syn
                                                                vs
                                                                (let* ([keyword-syn (hash-ref (parsed-attrs v) 'keyword)]
                                                                       [keyword-str (syntax->datum keyword-syn)])
                                                                  (cons (analysis-error (file-name)
                                                                                        (syntax-line   keyword-syn)
                                                                                        (syntax-column keyword-syn)
                                                                                        (format "~a is already specified at ~a:~a"
                                                                                                keyword-str
                                                                                                (syntax-line   first-one-syn)
                                                                                                (syntax-column first-one-syn)))
                                                                        res)))]))
  (let* ([first-one     (first lst)]
         [first-one-syn (hash-ref (parsed-attrs first-one) 'keyword)]
         [extra-ones    (rest  lst)])
    (parseds->analysis-error-repeated-helper first-one-syn extra-ones)))
                                                                                        
(define/contract (sync-ctxt->list vect)
  (-> (vector/c analysis-ctxt? list?)
      (vector/c analysis-ctxt? list?))
  (define/contract (sync-ctxt->list-helper ctxt-hash lst [res null])
    (->* (analysis-ctxt-hash? list?)
         (list?)
         list?)
    (match lst
      [(list)          (reverse res)]
      [(list v vs ...) (cond
                         [(or (not (parsed? v))
                              (symbol=? (parsed-time-level v) 'no-time-level))
                          (sync-ctxt->list-helper ctxt-hash vs (cons v res))]
                         
                         [else
                          (let* ([time-level     (parsed-time-level v)]
                                 [parsed-in-list v]
                                 [parsed-key     (string->symbol
                                                  (format "~a-parsed" time-level))]
                                 [parsed-in-ctxt (match (hash-ref ctxt-hash parsed-key)
                                                   [(list v) v]
                                                   [v        v])])
                            (cond
                              [(equal? parsed-in-list parsed-in-ctxt)
                               (sync-ctxt->list-helper ctxt-hash
                                                       vs
                                                       (cons parsed-in-list res))]

                              [else
                               (sync-ctxt->list-helper ctxt-hash
                                                       vs
                                                       (cons parsed-in-ctxt res))]))])]))
  (let* ([ctxt            (vector-ref vect 0)]
         [ctxt-hash       (analysis-ctxt->hash ctxt)]
         [parsed-list     (vector-ref vect 1)]
         [new-parsed-list (sync-ctxt->list-helper ctxt-hash parsed-list)])
    (vector ctxt new-parsed-list)))

(define/contract (sync-list->ctxt vect)
  (-> (vector/c analysis-ctxt? list?)
      (vector/c analysis-ctxt? list?))
  (define/contract (sync-list->ctxt-helper ctxt-hash lst [res ctxt-hash])
    (->* (analysis-ctxt-hash? list?)
         (analysis-ctxt-hash?)
         analysis-ctxt-hash?)
    (match lst
      [(list)          res]
      [(list v vs ...) (cond
                         [(or (not (parsed? v))
                              (symbol=? (parsed-time-level v) 'no-time-level))
                          (sync-list->ctxt-helper ctxt-hash vs res)]
                         
                         [else
                          (let* ([time-level     (parsed-time-level v)]
                                 [parsed-in-list v]
                                 [parsed-key     (string->symbol
                                                  (format "~a-parsed" time-level))]
                                 [parsed-in-ctxt (hash-ref ctxt-hash parsed-key)])
                            (cond
                              [(equal? parsed-in-list parsed-in-ctxt)
                               (sync-list->ctxt-helper ctxt-hash vs res)]

                              [else
                               (sync-list->ctxt-helper ctxt-hash
                                                       vs
                                                       (hash-set res
                                                                 parsed-key parsed-in-list))]))])]))
  (let* ([ctxt          (vector-ref vect 0)]
         [ctxt-hash     (analysis-ctxt->hash ctxt)]
         [parsed-list   (vector-ref vect 1)]
         [new-ctxt-hash (sync-list->ctxt-helper ctxt-hash parsed-list)])
    (vector (hash->analysis-ctxt new-ctxt-hash) parsed-list)))

(define/contract (sync-infos->ctxt vect)
  (-> (vector/c analysis-ctxt? list?)
      (vector/c analysis-ctxt? list?))
  (define/contract (accumulate-infos lst [res null])
    (->* (list?)
         (list?)
         list?)
    (match lst
      [(list)          res]
      [(list v vs ...) (cond
                         [(parsed? v) (let ([info-list (hash-ref (parsed-attrs v) 'info null)])
                                        (accumulate-infos vs
                                                          (append res
                                                                  info-list)))]
                         [else        (accumulate-infos vs res)])]))
  (let* ([ctxt        (vector-ref vect 0)]
         [parsed-list (vector-ref vect 1)]
         [ctxt-hash   (analysis-ctxt->hash   ctxt)]
         [info-list   (accumulate-infos      parsed-list)]
         [grouped-ht  (group-by-info-keyword info-list)])
    (vector (hash->analysis-ctxt (hash-set* ctxt-hash
                                            'year-info   (hash-ref grouped-ht "+year"  null)
                                            'month-info  (hash-ref grouped-ht "+month" null)
                                            'week-info   (hash-ref grouped-ht "+week"  null)
                                            'day-info    (hash-ref grouped-ht "+day"   null)
                                            'minute-info (hash-ref grouped-ht "+time"  null)))
            parsed-list)))

(define time-levels (list 'year
                          'month
                          'week
                          'day
                          'minute))

(define (time-level? x) (cond
                          [(member x time-levels) #t]
                          [else                   #f]))

(define/contract (info-time-level-by-keyword in)
  (-> (or/c syntax? string?)
      time-level?)
  (let ([keyword-str (cond
                       [(syntax? in) (syntax->datum in)]
                       [else         in])])
    (match keyword-str
      ["+year"  'year]
      ["+month" 'month]
      ["+week"  'week]
      ["+day"   'day]
      ["+time"  'minute])))

(define/contract (info-time-level in)
  (-> (cons/c syntax? syntax?)
      time-level?)
  (info-time-level-by-keyword (car in)))

(define/contract (sub-info-time-level-by-keyword in)
  (-> (or/c syntax? string?)
      time-level?)
  (let ([keyword-str (cond
                       [(syntax? in) (syntax->datum in)]
                       [else         in])])
    (match keyword-str
      ["+y" 'year]
      ["+m" 'month]
      ["+d" 'day]
      ["+t" 'minute]
      )))

(define/contract (sub-info-time-level in)
  (-> (cons/c syntax? syntax?)
      time-level?)
  (sub-info-time-level-by-keyword (car in)))

(define/contract (time-level-compare a b comp)
  (-> time-level? time-level? procedure?
      boolean?)
  (let ([a-pos (length (member a time-levels))]
        [b-pos (length (member b time-levels))])
    (comp a-pos b-pos)))

(define/contract (time-level<? a b)
  (-> time-level? time-level?
      boolean?)
  (time-level-compare a b <))

(define/contract (time-level<=? a b)
  (-> time-level? time-level?
      boolean?)
  (time-level-compare a b <=))

(define/contract (time-level>? a b)
  (-> time-level? time-level?
      boolean?)
  (time-level-compare a b >))

(define/contract (time-level>=? a b)
  (-> time-level? time-level?
      boolean?)
  (time-level-compare a b >=))

(define/contract (time-level=? a b)
  (-> time-level? time-level?
      boolean?)
  (time-level-compare a b =))

(define/contract (check-for-empty-infos lst [res null])
  (->* ((listof (cons/c syntax? syntax?)))
       ((listof analysis-error?))
       (listof analysis-error?))
  (match lst
    [(list)          (reverse res)]
    [(list v vs ...) (let* ([keyword-syn  (car v)]
                            [keyword-str  (syntax->datum keyword-syn)]
                            [info-syn     (cdr v)]
                            [info-str     (syntax->datum info-syn)]
                            [info-str-len (string-length info-str)])
                       (cond
                         [(= info-str-len 0) (check-for-empty-infos vs
                                                                    (cons (analysis-error (file-name)
                                                                                          (syntax-line   keyword-syn)
                                                                                          (syntax-column keyword-syn)
                                                                                          (format "~a is missing information"
                                                                                                  keyword-str))
                                                                          res))]
                         [else               (check-for-empty-infos vs res)]))]))

(define/contract (convert-info in)
  (-> (cons/c syntax? syntax?)
      any/c)
  (let* ([info-syn (cdr in)]
         [info-str (syntax->datum info-syn)])
    (match (info-time-level in)
      ['year   (string->year  info-str)]
      ['month  (string->month info-str)]
      ['week   (string->week  info-str)]
      ['day    (string->day   info-str)]
      ['minute (string->time  info-str)])))

(define/contract (convert-sub-info in)
  (-> (cons/c syntax? syntax?)
      any/c)
  (let* ([info-syn (cdr in)]
         [info-str (syntax->datum info-syn)])
    (match (sub-info-time-level in)
      ['year   (string->year  info-str)]
      ['month  (string->month info-str)]
      ['week   (string->week  info-str)]
      ['day    (string->day   info-str)]
      ['minute (string->time  info-str)])))
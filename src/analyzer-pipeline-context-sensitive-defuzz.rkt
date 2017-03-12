#lang racket

(require "analyzer-utils.rkt"
         "analyzer-components.rkt"
         "parsed-utils.rkt")

(provide context-sensitive-defuzz)

(define/contract (context-sensitive-defuzz vect)
  ; store results into both ctxt-ht and parsed struct attrs
  (-> (vector/c analysis-ctxt? (listof parsed?))
      (or/c (vector/c analysis-ctxt? (listof parsed?)) (listof analysis-error?)))
  (define/contract (context-sensitive-defuzz-single ctxt-hash x)
    (-> analysis-ctxt-hash? parsed?
        (or/c analysis-ctxt-hash? (listof analysis-error?)))
    (cond
      [(parsed-with-token x
                          '+YEAR
                          '+MONTH
                          '+WEEK
                          '+DAY
                          '+TIME)      (let ([ctxt-vect (hash-ref (parsed-attrs x) 'time)]
                                             [parsed-tl (parsed-time-level x)])
                                         (match ctxt-vect
                                           [(vector 'analysis-ctxt-free y)   (hash-set ctxt-hash parsed-tl (match y
                                                                                                             ['wildcard (recur-val  1)]
                                                                                                             [_         (single-val y)]))]
                                           [(vector 'analysis-ctxt-dep  lst) (let* ([keyword (hash-ref (parsed-attrs x) 'keyword)]
                                                                                    [res     (apply-analysis-ctxt-dep-λs ctxt-hash keyword lst)])
                                                                               (cond
                                                                                 [((listof analysis-error?) res) res]
                                                                                 [else                           (hash-set ctxt-hash parsed-tl (single-val res))]))]
                                           ['ambiguous                       (ambiguous->error-list (first (parsed-syntaxes x)))]
                                           ['no-match                        (no-match->error-list  (first (parsed-syntaxes x)))]))]
      [(parsed-with-token x '+FROM+TO) (let* ([unit           (list-ref (parsed-strings x) 0)]
                                              [ctxt-vect-from (hash-ref (parsed-attrs   x) 'from)]
                                              [ctxt-vect-to   (hash-ref (parsed-attrs   x) 'to)]
                                              [key            (match unit
                                                                ["year"   'year]
                                                                ["month"  'month]
                                                                ["week"   'week]
                                                                ["day"    'day]
                                                                ["time"   'minute])]
                                              [from-res (match ctxt-vect-from
                                                          [(vector 'analysis-ctxt-free y)   y]
                                                          [(vector 'analysis-ctxt-dep  lst) (let ([keyword (hash-ref (parsed-attrs x) 'keyword)])
                                                                                              (apply-analysis-ctxt-dep-λs ctxt-hash keyword lst))]
                                                          ['ambiguous                       (ambiguous->error-list (second (parsed-syntaxes x)))]
                                                          ['no-match                        (no-match->error-list  (second (parsed-syntaxes x)))])]
                                              [to-res   (match ctxt-vect-to
                                                          [(vector 'analysis-ctxt-free y)   y]
                                                          [(vector 'analysis-ctxt-dep  lst) (let ([keyword (hash-ref (parsed-attrs x) 'keyword)])
                                                                                              (apply-analysis-ctxt-dep-λs ctxt-hash keyword lst))]
                                                          ['ambiguous                       (ambiguous->error-list (third  (parsed-syntaxes x)))]
                                                          ['no-match                        (no-match->error-list  (third  (parsed-syntaxes x)))])])
                                         (let ([from-res-errors? ((listof analysis-error?) from-res)]
                                               [to-res-errors?   ((listof analysis-error?) to-res)])
                                           (cond
                                             [(and from-res-errors?
                                                   to-res-errors?)  (append from-res to-res)]
                                             [from-res-errors?      from-res]
                                             [to-res-errors?        to-res]
                                             [else                  (hash-set ctxt-hash key (ranged-val from-res to-res))])))]
      [(parsed-with-token x '+RECUR)   (let ([unit   (hash-ref (parsed-attrs x) 'unit)]
                                             [period (hash-ref (parsed-attrs x) 'period)])
                                         (hash-set ctxt-hash unit (recur-val period)))]
      [else                            ctxt-hash]))
  (define/contract (context-sensitive-defuzz-helper in lst)
    (-> (or/c analysis-ctxt-hash? (listof analysis-error?)) list?
        (or/c analysis-ctxt-hash? (listof analysis-error?)))
    (cond
      [((listof analysis-error?) in) in]
      [else
       (match lst
         [(list)          in]
         [(list v vs ...) (context-sensitive-defuzz-helper (context-sensitive-defuzz-single in v)
                                                           vs)])]))
  (let* ([ctxt (vector-ref vect 0)]
         [lst  (vector-ref vect 1)]
         [res  (context-sensitive-defuzz-helper (analysis-ctxt->hash ctxt) lst)])
    (cond
      [((listof analysis-error?) res) res]
      [else                           (vector (hash->analysis-ctxt res) lst)])))
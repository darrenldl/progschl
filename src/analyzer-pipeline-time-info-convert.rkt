#lang racket

(require "analyzer-components.rkt"
         "analyzer-utils.rkt"
         "parsed-utils.rkt")

(provide time-info-convert)

(define/contract (convert-info info)
  (-> (cons/c syntax? syntax?)
      (or/c (cons/c syntax? any/c) (listof analysis-error?)))
  (let* ([keyword-syn (car info)]
         [keyword-str (syntax->datum keyword-syn)]
         [info-syn    (cdr info)]
         [info-str    (syntax->datum info-syn)]
         [convert-res (convert-info info)])
    (match convert-res
      [(vector 'analysis-ctxt-free x) (let ([new-info (cons keyword-syn x)])
                                        (convert-infos vs
                                                       (vector (cons new-info
                                                                     (vector-ref res 0))
                                                               (vector-ref res 1))))]
      [(vector 'analysis-ctxt-dep  _) (convert-infos vs
                                                     (vector (vector-ref res 0)
                                                             (cons (analysis-error (file-name)
                                                                                   (syntax-line   info-syn)
                                                                                   (syntax-column info-syn)
                                                                                   "information is not static")
                                                                   (vector-ref res 1))))]
      ['ambiguous                     (convert-infos vs
                                                     (vector (vector-ref res 0)
                                                             (cons (analysis-error (file-name)
                                                                                   (syntax-line   info-syn)
                                                                                   (syntax-column info-syn)
                                                                                   "information is ambiguous")
                                                                   (vector-ref res 1))))]
      ['no-match                      (convert-infos vs
                                                     (vector (vector-ref res 0)
                                                             (cons (analysis-error (file-name)
                                                                                   (syntax-line   info-syn)
                                                                                   (syntax-column info-syn)
                                                                                   "information has no match")
                                                                   (vector-ref res 1))))])))

(define/contract (time-info-convert vect)
  (-> (vector/c analysis-ctxt? list?)
      (or/c (vector/c analysis-ctxt? list?) (listof analysis-error?)))
  (define/contract (time-info-convert-helper lst [res (vector null null)])
    (->* (list?)
         ((vector/c list? (listof analysis-error?)))
         (or/c list? (listof analysis-error?)))
    (match lst
      [(list)          (let ([parsed-list (vector-ref res 0)]
                             [error-list  (vector-ref res 1)])
                         (cond
                           [(empty? error-list) (reverse parsed-list)]
                           [else                error-list]))]
      [(list v vs ...) (cond
                         [(parsed? v) (time-info-convert-helper vs
                                                                (let ([convert-res (time-info-convert-single v)])
                                                                  (cond
                                                                    [(parsed? convert-res) (vector (cons convert-res
                                                                                                         (vector-ref res 0))
                                                                                                   (vector-ref res 1))]
                                                                    [else                  (vector (vector-ref res 0)
                                                                                                   (append (vector-ref res 1)
                                                                                                           convert-res))])))]
                         [else        (time-info-convert-helper vs (vector (cons v
                                                                                 (vector-ref res 0))
                                                                           (vector-ref 1)))])]))
  (let* ([ctxt        (vector-ref vect 0)]
         [parsed-list (vector-ref vect 1)]
         [res         (time-info-convert-helper parsed-list)])
    (cond
      [((listof analysis-error?) res) res]
      [else                           (sync-infos->ctxt
                                       (vector ctxt res))])))
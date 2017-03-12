#lang racket

(require "analyzer-components.rkt"
         "analyzer-utils.rkt"
         "parsed-utils.rkt")

(provide recur-sub-info-convert)

(define/contract (convert-sub-infos lst [res (vector null null)])
  (->* ((listof (cons/c syntax? syntax?)))
       ((vector/c (listof (cons/c syntax? any/c)) (listof analysis-error?)))
       (or/c (listof (cons/c syntax? any/c)) (listof analysis-error?)))
  (match lst
    [(list)          (let* ([sub-info-list (vector-ref res 0)]
                            [error-list    (vector-ref res 1)])
                       (cond
                         [(empty? error-list) (reverse sub-info-list)]
                         [else                (reverse error-list)]))]
    [(list v vs ...) (let* ([keyword-syn  (car v)]
                            [keyword-str  (syntax->datum keyword-syn)]
                            [sub-info-syn (cdr v)]
                            [sub-info-str (syntax->datum sub-info-syn)]
                            [convert-res  (convert-sub-info v)])
                       (match convert-res
                         [(vector 'analysis-ctxt-free x) (let ([new-sub-info (cons keyword-syn x)])
                                                           (convert-sub-infos vs
                                                                              (vector (cons new-sub-info
                                                                                            (vector-ref res 0))
                                                                                      (vector-ref res 1))))]
                         [(vector 'analysis-ctxt-dep  _) (convert-sub-infos vs
                                                                            (vector (vector-ref res 0)
                                                                                    (cons (analysis-error (file-name)
                                                                                                          (syntax-line   sub-info-syn)
                                                                                                          (syntax-column sub-info-syn)
                                                                                                          "sub-info is not static")
                                                                                          (vector-ref res 1))))]
                         ['ambiguous                     (convert-sub-infos vs
                                                                            (vector (vector-ref res 0)
                                                                                    (cons (analysis-error (file-name)
                                                                                                          (syntax-line   sub-info-syn)
                                                                                                          (syntax-column sub-info-syn)
                                                                                                          "sub-info is ambiguous")
                                                                                          (vector-ref res 1))))]
                         ['no-match                      (convert-sub-infos vs
                                                                            (vector (vector-ref res 0)
                                                                                    (cons (analysis-error (file-name)
                                                                                                          (syntax-line   sub-info-syn)
                                                                                                          (syntax-column sub-info-syn)
                                                                                                          "sub-info has no match")
                                                                                          (vector-ref res 1))))]))]))

(define/contract (recur-sub-info-convert vect)
  (-> (vector/c analysis-ctxt? list?)
      (or/c (vector/c analysis-ctxt? list?) (listof analysis-error?)))
  (define/contract (recur-sub-info-convert-single p)
    (-> parsed?
        (or/c parsed? (listof analysis-error?)))
    (cond
      [(parsed-with-token p
                          '+RECUR) (let* ([start@-info-list (hash-ref (parsed-attrs p) 'start@-info null)]
                                          [end@-info-list   (hash-ref (parsed-attrs p) 'end@-info   null)]
                                          [start@-res       (convert-sub-infos start@-info-list)]
                                          [end@-res         (convert-sub-infos end@-info-list)])
                                     (cond
                                       [(and (empty? start@-info-list)
                                             (empty? end@-info-list))  p]
                                       [(empty? start@-info-list)      (cond
                                                                         [((listof analysis-error?) end@-res)   end@-res]
                                                                         [else                                  (parsed-set-attrs p
                                                                                                                                  'end@-info   end@-res)])]
                                       [(empty? end@-info-list)        (cond
                                                                         [((listof analysis-error?) start@-res) start@-res]
                                                                         [else                                  (parsed-set-attrs p
                                                                                                                                  'start@-info start@-res)])]
                                       [else                           (cond
                                                                         [(and ((listof analysis-error?) start@-res)
                                                                               ((listof analysis-error?) end@-res)   (append start@-res
                                                                                                                             end@-res))]
                                                                         [((listof analysis-error?) start@-res)      start@-res]
                                                                         [((listof analysis-error?) end@-res)        end@-res]
                                                                         [else                                       (parsed-set-attrs p
                                                                                                                                       'start@-info start@-res
                                                                                                                                       'end@-info   end@-res)])]))]
      [else                        p]))
  (define/contract (recur-sub-info-convert-helper lst [res (vector null null)])
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
                         [(parsed? v) (recur-sub-info-convert-helper vs
                                                                     (let ([convert-res (recur-sub-info-convert-single v)])
                                                                       (cond
                                                                         [(parsed? convert-res) (vector (cons convert-res
                                                                                                              (vector-ref res 0))
                                                                                                        (vector-ref res 1))]
                                                                         [else                  (vector (vector-ref res 0)
                                                                                                        (append (vector-ref res 1)
                                                                                                                convert-res))])))]
                         [else        (recur-sub-info-convert-helper vs
                                                                     (vector (cons v
                                                                                   (vector-ref res 0))
                                                                             (vector-ref res 1)))])]))
  (let* ([ctxt        (vector-ref vect 0)]
         [parsed-list (vector-ref vect 1)]
         [res         (recur-sub-info-convert-helper parsed-list)])
    (cond
      [((listof analysis-error?) res) res]
      [else                           (sync-list->ctxt
                                       (vector ctxt res))])))
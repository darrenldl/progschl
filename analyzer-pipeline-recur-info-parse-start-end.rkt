#lang racket

(require "analyzer-components.rkt"
         "analyzer-utils.rkt"
         "parsed-utils.rkt")

(require data/monad
         data/applicative
         megaparsack megaparsack/text
         "parser-components.rkt")

(provide recur-info-parse-start-end)

(define-syntax (many-in-info/p stx)
  (syntax-case stx ()
    [(_ parser ...) #'(many/p (or/p (try/p blank-space-nonstrict/p)
                                    parser
                                    ...))]))

(define +y-info/p
  (sub-info/p "+y"))

(define +m-info/p
  (sub-info/p "+m"))

;(define +w-info/p
;  (sub-info/p "+w"))

(define +d-info/p
  (sub-info/p "+d"))

(define +t-info/p
  (sub-info/p "+t"))

(define time-info-list/p
  (many-in-info/p +y-info/p
                  +m-info/p
                  ;+w-info/p
                  +d-info/p
                  +t-info/p
                  unrecognized/p
                  ))

(define/contract (parse-sub-info syn)
  (-> syntax?
      (or/c (listof (cons/c syntax? any/c)) analysis-error?))
  (let* ([res (with-handlers ([exn:fail:read:megaparsack? package-error])
                (filter (cons/c syntax? syntax?)
                        (parse-result!
                         (parse-syntax-string time-info-list/p syn))))])
    (match res
      [(parse-error file-name
                    line
                    column
                    msg)      (analysis-error file-name
                                                    line
                                                    column
                                                    msg)]
      [_                      res])))

;(define/contract (parse-info-list lst [res (vector null null)])
;  (->* ((listof (cons/c syntax? any/c)))
;       ((vector/c (listof (cons/c syntax? any/c))
;                  (listof analysis-error?)))
;       (or/c (listof (cons/c syntax? any/c))
;             (listof analysis-error?)))
;  (match lst
;    [(list)          (let ([infos  (vector-ref res 0)]
;                           [errors (vector-ref res 1)])
;                       (cond
;                         [(empty? errors) infos]
;                         [else            errors]))]
;    [(list v vs ...) (let ([parse-res (recur-info-parse-info (second v))])
;                       (cond
;                         [((listof analysis-error?) parse-res) (parse-info-list vs (vector (vector-ref res 0)
;                                                                                           (append (vector-ref res 1)
;                                                                                                   parse-res)))]
;                         [else                                 (parse-info-list vs (vector (append 

(define/contract (parse-info-list lst [res (vector null null)])
  (->* ((listof (cons/c syntax? any/c)))
       ((vector/c (listof (cons/c syntax? any/c))
                  (listof analysis-error?)))
       (or/c (listof (cons/c syntax? any/c))
             (listof analysis-error?)))
  (match lst
    [(list)          (let ([infos  (vector-ref res 0)]
                           [errors (vector-ref res 1)])
                       (cond
                         [(empty? errors) infos]
                         [else            (reverse errors)]))]
    [(list v vs ...) (let* ([keyword-syn  (car v)]
                            [sub-info-syn (cdr v)]
                            [parse-res    (parse-sub-info sub-info-syn)])
                       (cond
                         [(analysis-error? parse-res) (parse-info-list vs (vector (vector-ref res 0)
                                                                                  (cons parse-res
                                                                                        (vector-ref res 1))))]
                         [else                        (parse-info-list vs (vector (append (vector-ref res 0)
                                                                                          parse-res)
                                                                                  (vector-ref res 1)))]))]))

(define/contract (recur-info-parse-start-end vect)
  (-> (vector/c analysis-ctxt? (listof parsed?))
      (or/c (vector/c analysis-ctxt? (listof parsed?)) (listof analysis-error?)))
  (define/contract (recur-info-parse-start-end-helper ctxt-hash)
    (-> analysis-ctxt-hash?
        (or/c analysis-ctxt-hash?
              (listof analysis-error?)))
    (let* ([start@-list (hash-ref ctxt-hash 'recur-all-start@-infos)]
           [end@-list   (hash-ref ctxt-hash 'recur-all-end@-infos)]
           [start@-res  (parse-info-list start@-list)]
           [end@-res    (parse-info-list end@-list)])
      (cond
        [(and (and ((listof analysis-error?) start@-res)
                   (not (empty? start@-res)))
              (and ((listof analysis-error?) start@-res)
                   (not (empty? start@-res))))           (append start@-res end@-res)]
        [(and ((listof analysis-error?) start@-res)
              (not (empty? start@-res)))                 start@-res]
        [(and ((listof analysis-error?) end@-res)
              (not (empty? end@-res)))                   end@-res]
        [else                                            (hash-set* ctxt-hash
                                                                    'recur-all-start@-infos start@-res
                                                                    'recur-all-end@-infos   end@-res)])))
;  (define/contract (recur-info-parse-start-end-single p)
;    (-> parsed?
;        (or/c parsed?
;              (listof analysis-error?)))
;    (let* ([start@-info          (hash-ref (parsed-attrs p) 'start@-info #f)]
;           [end@-info            (hash-ref (parsed-attrs p) 'end@-info   #f)]
;           [parsed-start@-info   (cond
;                                   [start@-info (recur-info-parse-info start@-info)]
;                                   [else        #f])]
;           [parsed-end@-info     (cond
;                                   [end@-info   (recur-info-parse-info end@-info)]
;                                   [else        #f])]
;           [parsed-start@-error? ((listof analysis-error?) parsed-start@-info)]
;           [parsed-end@-error?   ((listof analysis-error?) parsed-end@-info)])
;      (cond
;        [(and parsed-start@-error?
;              parsed-end@-error?)  (append parsed-start@-info
;                                           parsed-end@-info)]
;        [parsed-start@-error?      parsed-start@-info]
;        [parsed-end@-error?        parsed-end@-info]
;        [(and parsed-start@-info
;              parsed-end@-info)    (parsed-set-attrs p
;                                                     'start@-info parsed-start@-info
;                                                     'end@-info   parsed-end@-info)]
;        [parsed-start@-info        (parsed-set-attrs p
;                                                     'start@-info parsed-start@-info
;                                                     'end@-info   null)]
;        [parsed-end@-info          (parsed-set-attrs p
;                                                     'start@-info null
;                                                     'end@-info   parsed-end@-info)]
;        [else                      (parsed-set-attrs p
;                                                     'start@-info null
;                                                     'end@-info   null)])))
;  (define/contract (recur-info-parse-start-end-helper lst [res null])
;    (->* ((listof parsed?))
;         (list?)
;         (or/c (listof parsed?)
;               (listof analysis-error?)))
;    (match lst
;      [(list)          (cond
;                         [((listof parsed?) res) (reverse res)]
;                         [else                   res])]
;      [(list v vs ...) (let ([parsed-res (recur-info-parse-start-end-single v)])
;                         (cond
;                           [(and ((listof parsed?) res)         (parsed? parsed-res))
;                            (recur-info-parse-start-end-helper  vs
;                                                                (cons parsed-res res))]
;                           
;                           [(and ((listof analysis-error?) res) ((listof analysis-error?) parsed-res))
;                            (recur-info-parse-start-end-helper  vs
;                                                                (append res
;                                                                        parsed-res))]
;                           
;                           [((listof analysis-error?) res)
;                            (recur-info-parse-start-end-helper  vs
;                                                                res)]
;                           
;                           [((listof analysis-error?) parsed-res)
;                            (recur-info-parse-start-end-helper  vs
;                                                                parsed-res)]))]))
  (let* ([ctxt        (vector-ref vect 0)]
         [parsed-list (vector-ref vect 1)]
         [ctxt-hash   (analysis-ctxt->hash ctxt)]
         [res         (recur-info-parse-start-end-helper ctxt-hash)])
    (cond
      [((listof analysis-error?) res) res]
      [else                           (vector (hash->analysis-ctxt res) parsed-list)])))
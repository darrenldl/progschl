#lang racket

(require "analyzer-components.rkt"
         "parsed-utils.rkt")

(provide disable-recur-all-*-infos)

(define/contract (disable-recur-all-*-infos vect)
  (-> (vector/c analysis-ctxt? (listof parsed?))
      (vector/c analysis-ctxt? (listof parsed?)))
  (vector (analysis-ctxt-disable-fields (vector-ref vect 0)
                                        'recur-all-start@-infos
                                        'recur-all-end@-infos)
          (vector-ref vect 1)))
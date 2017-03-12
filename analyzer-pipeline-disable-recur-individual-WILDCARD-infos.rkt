#lang racket

(require "analyzer-components.rkt"
         "parsed-utils.rkt")

(provide disable-recur-individual-*-infos)

(define/contract (disable-recur-individual-*-infos vect)
  (-> (vector/c analysis-ctxt? (listof parsed?))
      (vector/c analysis-ctxt? (listof parsed?)))
  (vector (analysis-ctxt-disable-fields (vector-ref vect 0)
                                        'recur-individual-start@-infos
                                        'recur-individual-end@-infos)
          (vector-ref vect 1)))
#lang racket

(require "parsed-utils.rkt"
         "contexts-def.rkt"
         "contexts-def.rkt"
         "analyzer-components.rkt")

(provide print-branch-lists)

(define/contract (print-branch-lists in)
  (-> (listof (vector/c analysis-ctxt? list?))
      void?)
  (define (println-hash-wi hash)
    (for ([key (hash-keys hash)])
      (display "  ")
      (displayln (format "~a -> ~a" key (hash-ref hash key)))))
  (for ([i   (in-range (length in))]
        [vect in])
    (displayln "<<<<<<<<<<")
    (displayln (format "branch : ~a" i))
    (displayln ">>>>>>>>>>")
    (let* (;[time-gen-hash (time-point-gen->hash (vector-ref vect 0))]
           [ctxt-hash     (analysis-ctxt->hash  (vector-ref vect 0))]
           [parsed-list   (vector-ref vect 1)])
      ;(displayln "===== time point generator (converted to hash) =====")
      ;(println-hash-wi time-gen-hash)
      (displayln "=====   analysis context (converted to hash)   =====")
      (println-hash-wi ctxt-hash)
      (displayln "=====          list of parsed objects          =====")
      (for ([e parsed-list])
        (display "  |-> ")
        (writeln e))
      )))
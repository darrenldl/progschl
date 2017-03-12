#lang racket

(require "analyzer-components.rkt")

(provide make-time-point-gen)

(define/contract (make-time-point-gen vect)
  (-> (vector/c analysis-ctxt? list?)
      (or/c (vector/c time-point-gen? analysis-ctxt? list?)))
  (let ([ctxt        (vector-ref vect 0)]
        [parsed-list (vector-ref vect 1)]
        [time-gen    (hash->time-point-gen (hash))])
    (vector time-gen ctxt parsed-list)))
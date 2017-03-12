#lang racket

(require "defuzz-error-struct-def.rkt")

(provide show-defuzz-error)

(define (show-defuzz-error error)
  (-> defuzz-error?
      void?)
  (displayln (let ([fname (defuzz-error-fname error)])
               (if fname
                   (format "file : ~a, line : ~a, column : ~a -> ~a"
                           fname
                           (defuzz-error-line   error)
                           (defuzz-error-column error)
                           (defuzz-error-msg    error))
                   (format "line : ~a, column : ~a -> ~a"
                           (defuzz-error-line   error)
                           (defuzz-error-column error)
                           (defuzz-error-msg    error))))))
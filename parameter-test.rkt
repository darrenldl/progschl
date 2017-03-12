#lang racket

(require "global-parameters.rkt")

(define (t)
  (displayln (file-name)))

(module+ main
  (displayln (file-name))
  (t)

  (parameterize ([file-name "test"])
    (displayln (file-name))
    (t)
    )
  )
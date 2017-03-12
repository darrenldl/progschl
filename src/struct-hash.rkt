#lang racket

(require "macro-utils.rkt")

(define-syntax (define-for-struct-hash stx)
  (syntax-case stx ()
    [(define-for-struct-hash raw-name ([key key/c] ...))
     (with-syntax ([name        (format-id #'define-for-struct-hash "~a"      #'raw-name)]
                   [make-name   (format-id #'define-for-struct-hash "make-~a" #'raw-name)]
                   [keys-field  (first (generate-temporaries '(1)))]
                   [(mn-in ...) (generate-temporaries #'(key ...))])
       #'(begin
           (define-struct/contract name ([key        key/c]
                                         ...
                                         [keys-field set?]) #:transparent)

           (define (make-name mn-in ...)
             (name mn-in
                   ...
                   (set))
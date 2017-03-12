#lang racket

(provide parsed
         parsed-auto-id
         parsed?
         parsed-token
         parsed-id
         parsed-syntaxes
         parsed-strings
         parsed-content
         parsed-attrs)

(require "misc-utils.rkt")

(define (parsed-auto-id token syntaxes strings attrs content)
  (parsed token
          (gen-id)
          syntaxes
          strings
          attrs
          content))

(define-struct/contract parsed ([token    symbol?]
                                [id       exact-nonnegative-integer?]
                                [syntaxes (listof syntax?)]
                                [strings  (listof string?)]
                                [attrs    (and/c hash? immutable?)]
                                [content  list?])                   #:transparent)
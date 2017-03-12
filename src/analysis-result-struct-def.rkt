#lang racket

(define-struct/contract analysis-result ([type (or/c 'ctxt-free 'ctxt-dep)]
                                         [content (or/c 
#lang racket

(require "macro-utils.rkt"
         "date-time-utils.rkt"
         "string-utils.rkt"
         "parsed-utils.rkt"
         "time-point-struct-def.rkt"
         "contexts-def.rkt"
         "lambdas-def.rkt"
         "analysis-error-struct-def.rkt"
         "global-parameters.rkt"
         "time-point-gen-struct-def.rkt")

(provide analysis-ctxt-free-vect/c
         analysis-ctxt-free-vect
         analysis-ctxt-dep-vect/c
         analysis-ctxt-dep-vect
         
         (all-from-out "analysis-error-struct-def.rkt"
                       "contexts-def.rkt"
                       "lambdas-def.rkt"
                       "global-parameters.rkt"
                       "time-point-gen-struct-def.rkt"))

(define-syntax (analysis-ctxt-free-vect/c stx)
  (syntax-case stx ()
    [(_ res/c)       #'(vector/c 'analysis-ctxt-free res/c)]))

(define-syntax (analysis-ctxt-free-vect stx)
  (syntax-case stx ()
    [(_ content)     #'(vector 'analysis-ctxt-free content)]))

(define-syntax (analysis-ctxt-dep-vect/c stx)
  (syntax-case stx ()
    [_               #'(vector/c 'analysis-ctxt-dep (listof analysis-ctxt-dep-λ?))]))

(define-syntax (analysis-ctxt-dep-vect stx)
  (syntax-case stx ()
    [(_ content ...) #'(vector 'analysis-ctxt-dep (list content ...))]))
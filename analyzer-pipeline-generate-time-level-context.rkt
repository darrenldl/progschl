#lang racket

(require "analyzer-utils.rkt"
         "analyzer-components.rkt"
         "misc-utils.rkt"
         "parsed-utils.rkt")

(provide generate-time-level-context)

(define/contract (generate-time-level-context lst)
  (-> list?
      (vector/c analysis-ctxt? list?))
  (define/contract (generate-time-level-context-helper lst [res (hash)])
    (->* (list?)
         (hash?)
         hash?)
    (match lst
      [(list)          (reverse-lists-in-hash res)]
      [(list v vs ...) (cond
                         [(or (not (parsed? v))
                              (symbol=? (parsed-time-level v) 'no-time-level))
                          (generate-time-level-context-helper vs res)]
                         
                         [else
                          (let* ([tl  (parsed-time-level v)]
                                 [key (string->symbol (format "~a-parsed" tl))])
                            (generate-time-level-context-helper vs (hash-set res
                                                                             key (cons v (hash-ref res key null)))))])]))
  (vector (hash->analysis-ctxt (generate-time-level-context-helper lst)) lst))
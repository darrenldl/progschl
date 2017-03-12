#lang racket

(require "macro-utils.rkt"
         "parsed-utils.rkt"
         "analyzer-components.rkt"

         "analyzer-pipeline-empty-recur-sub-info-check.rkt"
         "analyzer-pipeline-delist-recur-sub-info.rkt"
         "analyzer-pipeline-duplicate-recur-sub-info-check.rkt"
         "analyzer-pipeline-disable-recur-all-WILDCARD-infos.rkt"
         "analyzer-pipeline-generate-recur-sub-info-context.rkt"
         "analyzer-pipeline-recur-info-parse-start-end.rkt"
         "analyzer-pipeline-disable-recur-individual-WILDCARD-infos.rkt"
         "analyzer-pipeline-duplicate-recur-info-check.rkt"
         "analyzer-pipeline-generate-recur-info-context.rkt"
         ;----
         "analyzer-pipeline-time-info-time-level-check.rkt"
         "analyzer-pipeline-delist-time-info.rkt"
         "analyzer-pipeline-duplicate-time-info-check.rkt"
         "analyzer-pipeline-generate-add-info-context.rkt"
         ;----
         "analyzer-pipeline-empty-info-check.rkt"
         ;----
         "analyzer-pipeline-context-sensitive-defuzz.rkt"
         "analyzer-pipeline-delist-parsed.rkt"
         "analyzer-pipeline-duplicate-time-level-check.rkt"
         "analyzer-pipeline-generate-time-level-context.rkt"
         "analyzer-pipeline-primary-defuzz.rkt"
         ;----
         "analyzer-pipeline-empty-from-to-string-check.rkt"
         "analyzer-pipeline-empty-block-head-string-check.rkt"
         
         "analyzer-pipeline-duplicate-tab-space-check.rkt")

(provide analyze-branches)

(define/contract (analyze-branch lst)
  (-> list?
      (or/c (vector/c analysis-ctxt? list?) (listof analysis-error?)))
  ((compose1-with-error-detection (listof analysis-error?) (; ## read the following from bottom to up ##
                                                            ;(REPLAN) time-point-gen-add-start-end-for-recur
                                                            ;(REPLAN) time-point-gen-add-fields
                                                            ;(TODO) make-time-point-gen
                                                            ; == create time point generator ==
                                                            ;(TODO) infer-missing-fields-analysis-ctxt
                                                            ;(TODO) patch-analysis-ctxt-using-time-info
                                                            ; -- patch missing fields in analysis context --
                                                            ;(REQUIRE REWRITE) recur-sub-info-convert
                                                            ;(REQUIRE REWRITE) time-info-convert
                                                            ; -- conversion --
                                                            delist-recur-sub-info
                                                            duplicate-recur-sub-info-check
                                                            disable-recur-all-*-infos
                                                            generate-recur-sub-info-context
                                                            ;(REQUIRE REWRITE) empty-recur-sub-info-check
                                                            recur-info-parse-start-end
                                                            disable-recur-individual-*-infos
                                                            duplicate-recur-info-check
                                                            generate-recur-info-context
                                                            ; -- recur specific info handling ---
                                                            time-info-time-level-check
                                                            delist-time-info
                                                            duplicate-time-info-check
                                                            generate-add-info-context
                                                            ; -- general time info handling --
                                                            empty-info-check
                                                            ; -- empty info check --
                                                            ; == info checking begins ==
                                                            context-sensitive-defuzz
                                                            delist-parsed
                                                            duplicate-time-level-check
                                                            generate-time-level-context
                                                            primary-defuzz
                                                            ; == parsed object defuzzing begins ==
                                                            empty-from-to-string-check
                                                            empty-block-head-string-check
                                                            ; == basic empty field check ==
                                                            ))
   lst))

(define/contract (analyze-branches lsts)
  (-> (listof list?)
      (or/c (listof (vector/c analysis-ctxt? (listof parsed?)))
            (listof analysis-error?)
            null))
  (let ([res ((compose1-with-error-detection (listof analysis-error?) (duplicate-tab-space-check)) lsts)])
    (cond
      [((listof analysis-error?) res) res]
      [else                           (cond
                                        [((listof null) lsts) null]
                                        [else
                                         (let* ([res    (map analyze-branch lsts)]
                                                [errors (foldr append '() (filter (listof analysis-error?) res))])
                                           (cond
                                             [(= (length errors) 0) res]
                                             [else                  errors]))])])))

(module+ test
  (analyze-branch (list
                   (parsed-auto-id '+MONTH
                                   (list (syntax "Jul"))
                                   (list "Jul")
                                   (hash 'keyword (syntax +month))
                                   null)
                   (parsed-auto-id '+YEAR
                                   (list (syntax "this"))
                                   (list "this")
                                   (hash 'keyword (syntax +year))
                                   null)
                   (parsed-auto-id '+RECUR
                                   (list (syntax "daily"))
                                   (list "daily")
                                   (hash 'keyword (syntax +recur)
                                         'cycle   "daily")
                                   null)
                   ))
  )
#lang racket

(require "file-parser.rkt"
         "analyzer.rkt"
         "global-parameters.rkt"
         "print-utils.rkt"
         "analyzer-utils.rkt"
         "analysis-error-struct-def.rkt")

(module+ main
  (parameterize ([file-name "test-run.pscl"])
    (define test-file "
+time 23:59 [+year 2018, +month June] {
}
+year 2019 [+day 28]{
  //+recur monthly [start@ +y 2020] {
  //}
}
+recur monthly [start@ +m July +t 13:99] {
  +recur daily [start@ +y 2018, end@ +y 2018] {}
}
"
      )

    (define in-file (open-input-file (file-name)))

    (define file-str (port->string in-file))

    (close-input-port in-file)

    (define parse-result (parse-file-string file-str))

    ;(print-parsed parse-result)

    (if (parse-error? parse-result)
        (show-parse-error parse-result)
        (begin
          ;(print-branch-lists (parsed->branch-lists parse-result))
          ;(newline)
          (let ([res (analyze-branches (parsed->branch-lists parse-result))])
            ;(newline)
            (cond
              [((listof analysis-error?) res) (show-analysis-errors res)]
              [else                           (print-branch-lists res)])
            (void)
            )
          )
        ;      (let ([branches (parsed->branch-lists parse-result)])
        ;        (let ([defuzzed-branches (defuzz-branch-lists branches)])
        ;          (if (defuzz-error? defuzzed-branches)
        ;              (show-defuzz-error defuzzed-branches)
        ;              (print-branch-lists defuzzed-branches)
        ;              )))
        )
    )
  )
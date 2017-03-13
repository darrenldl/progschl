#lang racket

(provide strict-space/p
         comma-andor-space/p
         escaped-brac/p
         
         blank-space/p
         blank-space+/p
         blank-space-nonstrict/p
         blank-space-nonstrict+/p
         
         newline/p
         line-end/p
         line-or-file-end/p
         end-with-char/p
         
         string-ci/p
         
         comment-string/p
         any-string/p
         file-name-string/p
         from-string/p
         add-info-string/p
         block-head-string/p
         time-block-head-string/p
         not-comma-string/p
         desc-string/p
         translation-original-string/p
         smallest-scope-string/p
         
         comma-sep/p

         add-info/p
         add-info-list/p

         sub-info/p
         
         unrecognized/p

         lockhead-parser
         brac/p

         one-of-strings/p
         one-of-strings-ci/p
         branched-block-string-prep
         branched-block-string/p

         hour-minute/p
         parse-hour-minute

         date-time-parser-gen

         comma-separated-strings/p
         comma-separated-parse/p
         comma-separated-syntax-strings/p

         package-error

         (all-from-out "parsed-struct-def.rkt"))

(require data/monad
         data/applicative
         megaparsack megaparsack/text)

(require "string-utils.rkt"
         "parsed-utils.rkt"
         "macro-utils.rkt"
         "parsed-struct-def.rkt"
         "global-parameters.rkt")

(define strict-space/p
  (satisfy/p char-blank?))

(define comma-andor-space/p
  (do (many/p strict-space/p)
      (char/p #\,)
      (many/p strict-space/p)))

(define escaped-space/p
  (try/p (do (char/p #\\)
             strict-space/p)))

(define escaped-brac/p
  (or/p (try/p (do (char/p #\\)
                   (char/p #\{)))
        (try/p (do (char/p #\\)
                   (char/p #\})))))

(define blank-space/p
  (do (many/p strict-space/p)
      (pure 'BLANK-SPACE)))

(define blank-space+/p
  (do (many+/p strict-space/p)
      (pure 'BLANK-SPACE)))

(define blank-space-nonstrict/p
  (do (many/p space/p)
      (pure 'BLANK-SPACE-NONSTRICT)))

(define blank-space-nonstrict+/p
  (do (many+/p space/p)
      (pure 'BLANK-SPACE-NONSTRICT)))

(define newline/p
  (label/p "newline"
           (char/p #\newline)))

(define comment-char/p
  (satisfy/p (λ (x)
               (and (char? x)
                    (not (equal? x #\newline))))))

(define comment-string/p
  (many/p comment-char/p))

(define-syntax (char-except/p stx)
  (syntax-case stx ()
    [(_ char ...) #'(satisfy/p (λ (x)
                                 (and (char? x)
                                      (not (equal? x char))
                                      ...)))]))

(define-syntax (char-escaped/p stx)
  (syntax-case stx ()
    [(_ char ...) #'(or/p (try/p (do (char/p #\\)
                                     (char/p char)))
                          ...)]))

(define-syntax (char-parser stx)
  (syntax-case stx ()
    [(_ (except ...) (escaped ...)) #'(or/p (try/p (char-escaped/p escaped
                                                                   ...))
                                            (char-except/p except
                                                           ...))]
    [(_ (except ...))               #'(or/p (try/p (char-escaped/p except
                                                                   ...))
                                            (char-except/p except
                                                           ...))]
    [(_)                            #'(or/p (char-except/p))]))

(define-syntax (lockhead-parser stx)
  (syntax-case stx ()
    [(_        (lockdown/p ...) (rest ...)) #'(do (try/p lockdown/p
                                                         ...)
                                                  rest
                                                  ...)]
    [(_ (head) (lockdown/p ...) (rest ...)) #'(do [head <- (try/p lockdown/p
                                                                  ...)]
                                                  rest
                                                  ...)]))

(define (brac/p parser)
  (lockhead-parser (start-brac)
                   ((syntax/p (char/p #\{)))
                   ([chunk    <- parser]
                    [end-brac <- (syntax/p (char/p #\}))]
                    (pure (hash 'chunk      chunk
                                'start-brac start-brac
                                'end-brac   end-brac)))))

(define-syntax (string-parser stx)
  (syntax-case stx ()
    [(_ char-parser) #'(do [list <- (many/p char-parser)]
                           (pure (list->string list)))]))

(define-syntax (string-parser+ stx)
  (syntax-case stx ()
    [(_ char-parser) #'(do [list <- (many+/p char-parser)]
                           (pure (list->string list)))]))

(define any-char/p
  (char-parser))

(define any-string/p
  (string-parser any-char/p))

(define block-head-char/p
  (char-parser (#\newline
                #\{
                #\}
                #\~
                #\+
                #\@
                #\!
                #\?
                )))

(define block-head-string/p
  (string-parser block-head-char/p))

(define time-block-head-char/p
  (char-parser (#\newline
                #\{
                #\}
                #\[
                #\]
                #\~
                #\+
                #\@
                #\!
                #\?
                )))

(define time-block-head-string/p
  (string-parser time-block-head-char/p))

(define add-info-char/p
  (char-parser (#\newline
                #\[
                #\]
                #\,
                )))

(define add-info-string/p
  (string-parser add-info-char/p))

(define-syntax (add-info/p stx)
  (syntax-case stx ()
    [(_ keyword-str) #'(lockhead-parser (keyword)
                                        ((syntax/p (string-ci/p keyword-str)))
                                        (blank-space/p
                                         [info <- (syntax/p add-info-string/p)]
                                         (pure (cons keyword info))))]))

(define sub-info-char/p
  (char-parser (#\newline
                #\[
                #\]
                #\,
                #\space
                )))

(define sub-info-string/p
  (string-parser sub-info-char/p))

(define-syntax (sub-info/p stx)
  (syntax-case stx ()
    [(_ keyword-str) #'(lockhead-parser (keyword)
                                        ((syntax/p (string-ci/p keyword-str)))
                                        (blank-space/p
                                         [info <- (syntax/p sub-info-string/p)]
                                         (pure (cons keyword info))))]))

(define-syntax (add-info-list/p stx)
  (syntax-case stx ()
    [(_ parser ...) #'(or/p (try/p (do (char/p #\[)
                                       blank-space-nonstrict/p
                                       (char/p #\])
                                       (pure null)))
                            (lockhead-parser ((char/p #\[))
                                             (blank-space-nonstrict/p
                                              [infos <- (comma-separated-parse/p parser
                                                                                 ...)]
                                              blank-space-nonstrict/p
                                              (char/p #\])
                                              [pure infos]))
                            (pure null))]))

(define smallest-scope-char/p
  (char-parser (#\{
                #\}
                #\newline)))

(define smallest-scope-string/p
  (string-parser smallest-scope-char/p))

(define desc-char/p
  (char-parser (#\})))

(define desc-string/p
  (string-parser desc-char/p))

(define not-comma-char/p
  (char-parser (#\,
                #\})))

(define not-comma-string/p
  (string-parser not-comma-char/p))

(define comma-sep/p
  (do blank-space-nonstrict/p
      (char/p #\,)
      blank-space-nonstrict/p))

(define from-char/p
  (char-parser (#\newline
                #\\
                #\+)))

(define from-string/p
  (string-parser from-char/p))

(define file-name-char/p
  (or/p letter/p
        digit/p
        symbolic/p
        escaped-space/p
        (char-in/p "_-+={}[]/")))

(define file-name-string/p
  (string-parser file-name-char/p))

(define line-end/p
  (do newline/p
      (pure 'LINE-END)))

(define line-or-file-end/p
  (or/p line-end/p
        eof/p))

(define translation-original-char/p
  (char-parser (#\{
                #\])))

(define translation-original-string/p
  (string-parser translation-original-char/p))

(define-syntax (end-with-char/p stx)
  (syntax-case stx ()
    [(_ char ...) #'(or/p (try/p (char/p char))
                          ...)]))

(define-syntax (optional-char/p stx)
  (syntax-case stx ()
    [(_ char ...) #'(try/p (or/p (try/p (char/p char))
                                 ...))]))

(define-syntax (one-of-strings/p stx)
  (syntax-case stx ()
    [(_ str ...) #'(or/p (try/p (string/p str))
                         ...)]))

(define-syntax (one-of-strings-ci/p stx)
  (syntax-case stx ()
    [(_ str ...) #'(or/p (try/p (string-ci/p str))
                         ...)]))

(define-syntax (branched-block-string-prep stx)
  (syntax-case stx ()
    [(_ str chunk-parser) #'(do (try/p (string/p str))
                                blank-space+/p
                                [chunk <- (brac/p chunk-parser)]
                                (pure (cons str chunk)))]))

(define-syntax (branched-block-string/p stx)
  (syntax-case stx ()
    [(_ (str ... chunk-parser) ...) #'(or/p (branched-block-string-prep str ...
                                                                        chunk-parser) ...)]))

(define comma-separated-strings/p
  (many/p (do [string <- not-comma-string/p]
              (pure (remove-head-tail-space string)))
          #:sep comma-sep/p))

(define comma-separated-syntax-strings/p
  (many/p (do [string <- (syntax/p not-comma-string/p)]
              (pure (datum->syntax string
                                   (remove-head-tail-space (syntax->datum string))
                                   string)))
          #:sep comma-sep/p))

(define-syntax (unrecognized/p stx)
  (syntax-case stx ()
    [(_ alt-parser) #'(guard/p alt-parser
                               (λ (x) #f))]
    [_              #'(guard/p smallest-scope-string/p
                               (λ (x) #f))]))

(define-syntax (comma-separated-parse/p stx)
  (syntax-case stx ()
    [(_ parser ...) #'(many/p (do blank-space-nonstrict/p
                                  [res <- (or/p parser
                                                ...)]
                                  blank-space-nonstrict/p
                                  (pure res))
                              #:sep comma-sep/p)]))

(define whatever-char/p
  (satisfy/p (λ (x) #t)))

(define hour-minute/p
  (do [hour   <- (guard/p integer/p
                          (λ (x) (<= x 23)))]
      blank-space/p
      (char/p #\:)
      blank-space/p
      [minute <- (guard/p integer/p
                          (λ (x) (<= x 59)))]
      (pure (cons hour minute))))

(define/contract (parse-hour-minute str)
  (-> string?
      (or/c (cons/c (integer-in 0 23) (integer-in 0 59)) #f))
  (with-handlers ([exn:fail:read:megaparsack? (λ (x) #f)])
    (parse-result!
     (parse-string hour-minute/p str))))

(define-syntax (date-time-parser-gen stx)
  (syntax-case stx ()
    [(date-time-parser-gen (name head token) ...)
     (with-syntax ([(parser-name  ...) (format-ids "~a" #'(name ...))])
       #'(begin
           (define parser-name
             (lockhead-parser ((string-ci/p head))
                              ([year   <- integer/p]
                               (char/p #\-)
                               [month  <- integer/p]
                               (char/p #\-)
                               [day    <- integer/p]
                               (char/p #\.)
                               [hour   <- integer/p]
                               (char/p #\:)
                               [minute <- integer/p]
                               (pure (parsed-auto-id token
                                                     null
                                                     null
                                                     (hash 'year   year
                                                           'month  month
                                                           'day    day
                                                           'hour   hour
                                                           'minute minute)
                                                     null)))))
           ...))]))

;(define translation/p
;  (do [string <- any-string/p]
;      (pure (if (regexp-match #rx"<-" string)
;                (let ([result   (regexp-replace #rx"(.*)<-(.*)" string "\\1")]
;                      [original (regexp-replace #rx"(.*)<-(.*)" string "\\2")])
;                  (cons (remove-head-tail-space original)
;                        (remove-head-tail-space result)))
;                (cons (remove-head-tail-space string)
;                      #f)))))

(define (string-ci/p str)
  (-> string? parser?)
  (let ([str-len (string-length str)])
    (do [syntax-chars <- (or/p (try/p (repeat/p str-len (syntax/p whatever-char/p)))
                               (many/p (syntax/p whatever-char/p)))]
        (if (= 0 (length syntax-chars))
            (fail/p
             (message (srcloc #f #f #f #f #f)
                      "end of input"
                      (list str)))
            (let* ([chars             (map syntax->datum syntax-chars)]
                   [matched           (string-match  #:ci? #t str chars)]
                   [parsed-str        (list->string  chars)]
                   [parsed-str-len    (string-length parsed-str)])
              (cond
                [(= matched str-len) (pure parsed-str)]
                [(< matched str-len) (let* ([last-matched      (list-ref syntax-chars (if (= matched 0)
                                                                                          0
                                                                                          (sub1 matched)))]
                                            [make-error-srcloc (λ (#:eof? eof)
                                                                 (srcloc 'string
                                                                         (syntax-line   last-matched)
                                                                         (if (= matched 0)
                                                                             0
                                                                             (syntax-column last-matched))
                                                                         (if (= matched 0)
                                                                             1
                                                                             (syntax-position last-matched))
                                                                         (if eof
                                                                             1
                                                                             2)))]
                                            )
                                       (if (< matched parsed-str-len)
                                           (fail/p
                                            (message (make-error-srcloc #:eof? #f)
                                                     (string-ref parsed-str matched)
                                                     (list
                                                      (substring str matched str-len))))
                                           (fail/p
                                            (message (make-error-srcloc #:eof? #t)
                                                     "end of input"
                                                     (list
                                                      (substring str parsed-str-len str-len))))))]
                ))
            )
        )))

(define/contract (package-error error)
  (-> exn:fail:read:megaparsack?
      parse-error?)
  (match error
    [(struct exn:fail:read:megaparsack
       (_ _ srclocs unexpected expected)) (let ([srcloc (first srclocs)])
                                            (parse-error (file-name)
                                                         (srcloc-line   srcloc)
                                                         (add1 (srcloc-column srcloc))
                                                         (format "unexpected/unrecognized : ~a"
                                                                 unexpected)))]))

(module+ test
;  (parse-result! (parse-string whatever-char/p "ab"))
;
;  (define pattern "Abc")
;  (define input   "Abc")
;  
;  (define string-parse    (parse-string (do (char/p #\space)
;                                            (syntax/p (string/p    pattern))) (string-append " " input)))
;  string-parse
;  
;  (define string-ci-parse (parse-string (do (char/p #\space)
;                                            (syntax/p (string-ci/p pattern))) (string-append " " input)))
;  string-ci-parse
;
;  (define string-ci-parse2 (parse-string (string-ci/p "abd") "Abe"))
;  string-ci-parse2
;
;  (parse-string integer/p "k123")

  (parse-string (do [k <- (brac/p integer/p)]
                    (pure (hash-ref k 'start-brac))) "{123")

  ;(parse-string translation/p "8:00am - 10:00am <- 8:00 am, 2 hours")
  )

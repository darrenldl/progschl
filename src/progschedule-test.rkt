#lang racket

(require datalog)

(require (for-syntax racket/syntax
                     racket/base))

(define schedule (make-theory))

(define-syntax (concrete-entry stx)
  (syntax-case stx ()
    [(_         id name y m d s-h s-m e-h e-m)
     #'(conrete id name y m d s-h s-m e-h e-m)]))

(datalog schedule
         (! (concrete 0 "COMP XXXX\nLecture" 2017 2 17 9 0 10 #,'a))
         (! (concrete 1 "MATH XXXX\nLecture" 2017 2 17 9 0 11 00))
         (! (concrete 2 "COMP XXXX\nLecture" 2017 2 19 9 0 10 30))
         (! (concrete 2 "COMP XXXX\nLecture" 2017 2 19 9 0 10 30))
         (? (concrete Id Name Y M D S-H S-M E-H a)))

(newline)

(datalog schedule
         (? (concrete Id Name 2017 2 17 S-H S-M E-H E-M)))

(newline)

(let ()
  (define (hours-overlap SH1 EH1 SH2 EH2)
    (> EH1 SH2))
  (datalog schedule
           (! (:- (collide Name1 Name2 Y M)
                  (concrete Id1 Name1 Y M D SH1 SM1 EH1 EM1)
                  (concrete Id2 Name2 Y M D SH2 SM2 EH2 EM2)
                  (hours-overlap SH1 EH1 SH2 EH2 :- #t)
                  (< Id1 Id2 :- #t)
                  ))
  ))

(define-syntax (test stx)
  (syntax-case stx ()
    [(_) #'(datalog schedule
                    (? (collide First Second 2017 2)))]))

(datalog schedule
         (? (collide First Second 2017 2)))

(test)

(define-syntax (define-for-print stx)
  (syntax-case stx ()
    [(_ name)
     (with-syntax ([func-id (format-id stx "print-~a" #'name)])
       #'(define (func-id)
           (displayln "a")))]))


(whatever whoijafd)

(define-syntax (whatever stx)
  (syntax-case stx ()
    [(_) #'(whatever abcd)]
    [(whatever k ...)
     (with-syntax* ([one-of (list-ref (syntax->list #'(k ...)) 0)]
                    [func-id (format-id #'whatever "print-a")])
       #'(begin
           (define (func-id)
             (displayln "a"))))]))


(print-a)
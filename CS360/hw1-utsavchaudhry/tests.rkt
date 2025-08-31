#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "scheme.rkt")

(provide scheme-tests)

(define (square x) (* x x))

(define scheme-tests
  (test-suite
   "Scheme Homework Tests"
   
   (test-suite
    "falling"
    (test-case "(falling 10 0)"
               (check-equal? (falling 10 0)
                             1))
    (test-case "(falling 10 1)"
               (check-equal? (falling 10 1)
                             10))
    (test-case "(falling 10 8)"
               (check-equal? (falling 10 8)
                             1814400)))

   (test-suite
    "S"
    (test-case "(S 10 10)"
               (check-equal? (S 10 10)
                             1))
    (test-case "(S 10 0)"
               (check-equal? (S 10 0)
                             0))
    (test-case "(S 0 10)"
               (check-equal? (S 0 10)
                             0))
    (test-case "(S 10 7)"
               (check-equal? (S 10 7)
                             5880)))

   (test-suite
    "zip"
    (test-case "(zip null null)"
               (check-equal? (zip null null)
                             null))
    
    (test-case "(zip '(1 2 3) '(a b c))"
               (check-equal? (zip '(1 2 3) '(a b c))
                             '((1 . a) (2 . b) (3 . c)))))

   (test-suite
    "pow"
    (test-case "((pow square 0) 10)"
               (check-equal? ((pow square 0) 10)
                             10))
   
    (test-case "((pow square 1) 10)"
               (check-equal? ((pow square 1) 10)
                             100))
   
    (test-case "((pow square 2) 10)"
               (check-equal? ((pow square 2) 10)
                             10000)))

   (test-suite
    "argmax"
    (test-case "argmax base case"
               (check-equal? (argmax (lambda (x) (* x x)) '(0))
                             0))

    (test-case "argmax negate"
               (check-equal? (argmax (lambda (x) (- x)) '(0 100 -55 32 64 -21))
                             -55))

    (test-case "argmax square"
               (check-equal? (argmax (lambda (x) (* x x)) '(1 2 3 4 0 -1 -2 -3 -4 -5))
                             -5)))))

#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "mceval.rkt")

(define (test-mceval exp)
  (with-handlers ([exn:fail? (lambda (exn) exn)])
    (mceval exp (setup-environment))))

(define primitive-tests
  (test-suite
   "Test primitives"
   (check-equal? (test-mceval '(* 4 5))
                 20
                 "Implement *")
   (check-equal? (test-mceval '(- 4 5))
                 -1
                 "Implement -")
   (check-equal? (test-mceval '(+ 4 5))
                 9
                 "Implement -")
   (check-equal? (test-mceval '(= 4 4))
                 #t
                 "Implement =")
))

(define let-tests
  (test-suite
   "Test the let special forms"
   (check-equal? (test-mceval '(let ((x 1) (y 2)) (+ x y)))
                 3
                 "(let ((x 1) (y 2)) (+ x y))")))

(define dynamic-eval-tests
  (test-suite
   "Check dynamic vs. static scope"
   (check-equal? (test-mceval '(begin (define (make-adder x) (lambda (y) (+ x y)))
                                      (define add1 (make-adder 1))
                                      (define x 2)
                                      (add1 2)))
                 3
                 "(add1 2) - static scope")))

(run-tests primitive-tests)
(run-tests let-tests)
(run-tests dynamic-eval-tests)

#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "mceval-tests.rkt")

(provide mceval-tests)

(define mceval-tests
  (test-suite
   "Metacircular Evaluator Tests"
   basic-tests
   null-tests
   primitive-tests
   not-tests
   let-tests))

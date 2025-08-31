#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "intro.rkt")

(provide intro-tests)

(define intro-tests
  (test-suite
   "Intro Homework Tests"

   (test-suite
    "add1"
    (test-case "(add1 1)"
               (check-equal? (add1 1)
                             2))
    (test-case "(add1 99)"
               (check-equal? (add1 99)
                             100)))))

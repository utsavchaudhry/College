#lang racket
(require rackunit/text-ui)
(require "tests.rkt")

(when (not (eq? (run-tests more-scheme-tests) 0))
    (exit 1))

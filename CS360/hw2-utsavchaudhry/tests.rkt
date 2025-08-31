#lang racket
(require racket/sandbox)
(require rackunit)
(require rackunit/text-ui)
(require "more-scheme.rkt")

(provide more-scheme-tests)

(define (integers-from n)
  (stream-cons n (integers-from (+ n 1))))

(define integers
  (integers-from 0))

(define more-scheme-tests
  (test-suite
   "More Scheme Tests"

   stream-pair-with-tests
   stream-iterate-tests
   stream-zip-tests
   cycle-streams-tests
   count-even-tests))

(define stream-pair-with-tests
  (test-suite
   "stream-pair-with"
   
   (test-case
    "(stream-pair-with (lambda (x) (+ x 1)) (1 2 3 4))"
    (check-equal? (stream->list (stream-pair-with (lambda (x) (+ x 1)) (stream 1 2 3 4)))
                  '((1 . 2) (2 . 3) (3 . 4) (4 . 5))))
   
   (test-case
    "(stream-pair-with (lambda (x) (+ x 1)) (integers-from 1))"
    (check-equal? (stream->list (stream-take (stream-pair-with (lambda (x) (+ x 1)) (integers-from 1)) 4))
                  '((1 . 2) (2 . 3) (3 . 4) (4 . 5))))))

(define (approx eps x)
  (lambda (y) (< (abs (- x y)) eps)))

(define stream-iterate-tests
   (test-suite
    "stream-iterate"
    
    (test-case
     "stream-iterate of inc function"
     (check-equal? (stream-ref (stream-iterate (lambda (x) (+ 1 x)) 0) 100)
                   100))
    
    (test-case
     "stream-iterate compute golden ratio"
     (check-pred (approx 0.001 (/ (+ 1 (sqrt 5)) 2))
                 (stream-ref (stream-iterate (lambda (x) (+ 1 (/ 1 x))) 1) 100)))))

(define stream-zip-tests
   (test-suite
    "stream-zip"
    
    (test-case
     "stream-zip base case 1"
     (check-equal? (stream->list (stream-zip (stream 1) empty-stream))
                   '()))
    
    (test-case
     "stream-zip base case 2"
     (check-equal? (stream->list (stream-zip empty-stream (stream 1)))
                   '()))
    
    (test-case
     "stream-zip finite streams"
     (check-equal? (stream->list (stream-zip (stream 1 2 3) (stream 'a 'b 'c)))
                   '((1 . a) (2 . b) (3 . c))))
    
    (test-case
     "stream-zip streams of different length"
     (check-equal? (stream->list (stream-zip (stream 1 2 3) (stream 'a 'b)))
                   '((1 . a) (2 . b))))
    
    (test-case
     "stream-zip infinite streams"
     (check-equal? (stream->list (stream-take (stream-zip integers integers) 3))
                   '((0 . 0)  (1 . 1) (2 . 2))))
    
    (test-case
     "stream-zip proper time/space complexity"
     (check-equal? (stream-ref (stream-zip integers integers) 10000)
                   '(10000 . 10000)))))

(define cycle-streams-tests
  (test-suite
   "cycle-streams"
   
   (test-case
    "cycle-streams finite streams"
    (check-equal? (stream->list (stream-take (cycle-streams '(1 2 3) '("a" "b")) 8))
                  '((1 . "a") (2 . "b") (3 . "a") (1 . "b") (2 . "a") (3 . "b") (1 . "a") (2 . "b"))))
   
   (test-case
    "cycle-streams infinite streams"
    (check-equal? (stream->list (stream-take (cycle-streams (integers-from 0) (integers-from 1)) 8))
                  '((0 . 1) (1 . 2) (2 . 3) (3 . 4) (4 . 5) (5 . 6) (6 . 7) (7 . 8))))
    
    (test-case
     "cycle-streams proper time/space complexity"
     (check-equal? (stream-ref (cycle-streams integers integers) 10000)
                   '(10000 . 10000)))))

;; Run count-even in a sandbox so we get fresh state every time
(define (map-count-even xs)
  (let [(sandbox-eval (make-evaluator #:requires '("more-scheme.rkt")
                                      'racket))]
    (map (lambda (x) (sandbox-eval `(count-even (quote ,x)))) xs)))

(define count-even-tests
   (test-suite
    "count-even"
    
    (test-case
     "count-even single odd"
     (check-equal? (map-count-even '(1))
                   '(0)))
    
    (test-case
     "count-even single even"
     (check-equal? (map-count-even '(2))
                   '(1)))
    
    (test-case
     "count-even"
     (check-equal? (map-count-even '(1 2 4 5 9 22 -10 -21))
                   '(0 1 2 2 2 3 4 4)))))

;;
;; DO NOT REMOVE THESE TWO LINES
;;
#lang racket
(provide (all-defined-out))
(require racket/stream)

;;
;; Problem 1
;;
;; A stream-of-pairs is one of
;; - the empty stream
;; - stream-cons of a pair and a stream
;;
;; (Any -> Any) Stream -> Stream-of-pairs
;;
;; Given a function f and a stream s, return a stream consisting of each element
;; x of the stream s paired with (f x).
;;

(define (stream-pair-with f s)
  (stream-map (lambda (x) (cons x (f x))) s))

;;
;; Problem 2
;;
;; (Any -> Any) Any -> Stream
;;
;; Given a function and a value x, return am infinite stream consisting of
;; repeated applications of f to x. The elements of the stream are x, (f x),
;; (f (f x)), ...
;;

(define (stream-iterate f x)
  (stream-cons x (stream-iterate f (f x))))

;;
;; Problem 3
;;
;; Stream Stream -> Stream
;;
;; zip for streams. Return a stream whose elements are pairs where the first
;; item in the pair is taken from xs and the second item in the pair is taken
;; from ys.
;;

(define (stream-zip xs ys)
  (if (or (stream-empty? xs) (stream-empty? ys))
      empty-stream
      (stream-cons
        (cons (stream-first xs) (stream-first ys))
        (stream-zip (stream-rest xs) (stream-rest ys)))))


;;
;; Problem 4
;;
;; Stream Stream -> Stream
;;
;; Return a stream whose elements are pairs where the first item in the pair is
;; taken from xs and the second item in the pair is taken from ys. If xs or ys
;; is finite and runs out of element, start back at the beginning of the stream.
;;
;; Note that xs and ys may or may not be infinite, and they may or may not have
;; the same length if they are finite.
;; 

(define (cycle-stream xs)
  (let ([orig xs])
    (define (helper s)
      (if (stream-empty? s)
          (helper orig)
          (stream-cons (stream-first s)
                       (helper (stream-rest s)))))
    (helper xs)))

(define (cycle-streams xs ys)
  (stream-zip (cycle-stream xs)
              (cycle-stream ys)))

;;
;; Problem 5
;;
;; Number -> Integer
;;
;; Return the number of times the function has been called with an even
;; argument, including the current argument.
;;

(define count-even
  (let ([cnt 0])
    (lambda (x)
      (when (even? x) (set! cnt (add1 cnt)))
      cnt)))


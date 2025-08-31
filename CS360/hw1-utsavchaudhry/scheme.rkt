;;
;; DO NOT REMOVE THESE TWO LINES
;;
#lang racket
(provide (all-defined-out))

;;
;; Problem 1
;;
;; Number Integer -> Number
;;
;; Compute the falling factorial x to the n falling
;;
(define (falling x n)
  (if (zero? n)
      1
      (* x (falling (- x 1) (- n 1)))))

;;
;; Problem 2
;;
;; Integer Integer -> Integer
;;
;; Compute the Stirling number of the second kind, n subset k
;;
(define (S n k)
  (cond
    [(and (= n 0) (= k 0)) 1]
    [(and (> n 0) (= n k)) 1]
    [(and (> n 0) (= k 0)) 0]
    [(or (< k 0) (> k n)) 0]
    [else
     (+ (* k (S (sub1 n) k))
        (S (sub1 n) (sub1 k)))]))


;;
;; Problem 3
;;
;; List List -> List
;;
;; Produce a list of pairs where the first element of each pair is take from
;; the first arguments xs and the second element of each pair is taken from the
;; second argument, ys.
;;

(define (zip xs ys)
  (cond
    [(and (null? xs) (null? ys)) '()]
    [(or (null? xs) (null? ys))
     (error 'zip "Lists must have the same length")]
    [else
     (cons
       (cons (car xs) (car ys))
       (zip (cdr xs) (cdr ys)))]))



;;
;; Problem 4
;;
;; ((Any -> Any) -> Integer) -> (Any -> Any)
;;
;; Compute f composed with itself n times.
;; (pow f 0) is the identity function
;; (pow f 1) is f
;;

(define (pow f n)
  (cond
    [(zero? n)    identity]
    [else         (compose f (pow f (sub1 n)))]))


;;
;; Problem 5
;;
;; (Any -> Number) -> List -> Any
;;
;; Compute the element x in xs for which (f x) is greatest.
;;

;; argmax : (Any -> Number) -> Listof Any -> Any
(define (argmax f xs)
  (let loop ([ys (cdr xs)]
             [best (car xs)]
             [bval (f (car xs))])
    (if (null? ys)
        best    
        (let* ([x   (car ys)]
               [vx  (f x)])    
          (if (> vx bval)
              (loop (cdr ys) x vx) 
              (loop (cdr ys) best bval))))))


#lang racket
(require rackunit)
(require "mceval.rkt")

(provide basic-tests
         null-tests
         primitive-tests
         not-tests
         let-tests)

(define (test-mceval exp)
  (with-handlers ([exn:fail? (lambda (exn) exn)])
    (mceval exp (setup-environment))))

(define (test-mceval-sequence . exps)
  (with-handlers ([exn:fail? (lambda (exn) exn)])
    (let ((env (setup-environment)))
      (define (loop exps)
        (if (null? (cdr exps))
            (mceval (car exps) env)
            (begin (mceval (car exps) env)
                   (loop (cdr exps)))))
      (loop exps))))

(define (test-mceval-exception exp)
  (mceval exp (setup-environment)))

(define basic-tests
  (test-suite
   "Basic tests"
   
   (test-case
    "quote"
    (check-equal?
     (test-mceval '(quote (a b c)))
     '(a b c)))
   
   (test-case
    "set!"
    (check-equal?
     (test-mceval '(begin (define x 0) (set! x 1) x))
     1))
   
   (test-case
    "if"
    (check-equal?
     (test-mceval '(if false false true))
    #t))
   
   (test-case
    "cond"
    (check-equal?
     (test-mceval '(cond (false 0) (true 1)))
    1))
   
   (test-case
    "Lambda application"
    (check-equal?
     (test-mceval '((lambda (x) x) 1))
     1))
   
   (test-case
    "Function application"
    (check-equal?
     (test-mceval '(begin (define (id x) x) (id 1)))
     1))))

(define null-tests
  (test-suite
   "Problem 1: Adding `null` to the interpreter"
   (test-case "null is '()"     (check-equal? (test-mceval 'null)
                                              '()))
   (test-case "(null? null)"    (check-equal? (test-mceval '(null? null))
                                              #t))))

(define primitive-tests
  (test-suite
   "Problem 2: Adding primitives"
   (test-case "Implement +"     (check-equal? (test-mceval '(+ 4 5))
                                              9))
   (test-case "Implement -"     (check-equal? (test-mceval '(- 4 5))
                                              -1))
   (test-case "Implement *"     (check-equal? (test-mceval '(* 4 5))
                                              20))
   (test-case "Implement /"     (check-equal? (test-mceval '(/ 8 4))
                                              2))
   (test-case "Implement <"     (check-equal? (test-mceval '(< 4 4))
                                              #f))
   (test-case "Implement <="    (check-equal? (test-mceval '(<= 4 4))
                                              #t))
   (test-case "Implement ="     (check-equal? (test-mceval '(= 4 4))
                                              #t))
   (test-case "Implement >="    (check-equal? (test-mceval '(>= 4 4))
                                              #t))
   (test-case "Implement >"     (check-equal? (test-mceval '(> 4 4))
                                              #f))
   (test-case "Implement error" (check-exn (regexp "^Metacircular Interpreter Aborted$")
                                           (lambda () (test-mceval-exception '(error)))))))

(define not-tests
  (test-suite
   "Problem 3: Adding `not` to the interpreter"
   (test-case "(not true)"
              (check-equal? (test-mceval '(not true))
                            #f))
   
   (test-case "(not false)"
              (check-equal? (test-mceval '(not false))
                            #t))
   
   (test-case "(not (if true false true))"
              (check-equal? (test-mceval '(not (if true false true)))
                            #t))
   ))

(define let-tests
  (test-suite
   "Problem 4: let"
   (test-case "(let () \"unused\" 2)"
              (check-equal? (test-mceval '(let () "unused" 2))
                            2))
   (test-case "(let ((x 1) (y 2)) (+ x y))"
              (check-equal? (test-mceval '(let ((x 1) (y 2)) (+ x y)))
                            3))
   
   (test-case "(let ((x (+ 1 3)) (y (* 2 5))) (+ x y))"
              (check-equal? (test-mceval '(let ((x (+ 1 3)) (y (* 2 5))) (+ x y)))
                            14))
   
   (test-case "(let ((x 1) (y 2)) (set! x 2) (+ x y))"
              (check-equal? (test-mceval '(let ((x 1) (y 2)) (set! x 2) (+ x y)))
                            4))
   
   (test-case "(let ((x 1)) (let ((y 2)) (set! x 2) (+ x y)))"
              (check-equal? (test-mceval '(let ((x 1)) (let ((y 2)) (set! x 2) (+ x y))))
                            4))))

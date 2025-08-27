#lang racket/base
(require racket/list
         racket/eval)

(define (cond_to_if exp)
  (let ([first-exp (first exp)]
        [rest-exp  (rest exp)])
    ;; now actually evaluate the “then-branch” expression
    (eval (rest first-exp)
          (interaction-environment))))



(define (foo x)
  (list
    ;; first if: positive? vs non-positive?
    (if (> x 0) 'positive 'non-positive)
    ;; second if: small? vs not-small?
    (if (< x 10) 'small 'not-small)
    ;; third if: even? vs odd?
    (if (even? x) 'even 'odd)))

(define (make-greater-than n)
  ;; The body is a single expression: the lambda itself.
  ;; That lambda, when called on x, will perform the 'if' test.
  (lambda (x)
    (if (> x n)
        #t
        #f)))



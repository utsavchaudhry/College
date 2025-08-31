#lang racket/base
(require (except-in racket force delay))
(require racket/mpair)

;;;; Ported to Racket by Geoffrey Mainland <mainland@drexel.edu>
;;;; METACIRCULAR EVALUATOR FROM CHAPTER 4 (SECTIONS 4.1.1-4.1.4) of
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;; Evaluate a top-level expression using top-mceval

;;; SECTION 4.1.1

(define (mceval exp env)
  (cond
    ((self-evaluating? exp) exp)
    ((variable? exp)        (lookup-variable-value exp env))
    ((quoted? exp)          (text-of-quotation exp))
    ((assignment? exp)      (eval-assignment exp env))
    ((definition? exp)      (eval-definition exp env))
    ((if? exp)              (eval-if exp env))
    ((lambda? exp)
     (make-procedure (lambda-parameters exp)
                     (lambda-body exp)
                     env))
    ((begin? exp)           (eval-sequence (begin-actions exp) env))
    ((cond? exp)            (mceval (cond->if exp) env))
    ((and? exp)            (mceval (and->if exp) env))
    ((or? exp)            (mceval (or->if exp) env))
    ((let? exp)             (mceval (let->application exp) env))
    ((application? exp)
     (mcapply (mceval (operator exp) env)
              (list-of-values (operands exp) env)))
    (else
     (error "mceval: Unknown expression type" exp))))

(define (mcapply procedure arguments)
  (cond
    ((primitive-procedure? procedure)
     (apply-primitive-procedure procedure arguments))
    ((compound-procedure? procedure)
     (eval-sequence
       (procedure-body procedure)
       (extend-environment
        (procedure-parameters procedure)
        arguments
        (procedure-environment procedure))))
    (else
     (error "mcapply: Unknown procedure type" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (mceval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (mceval (if-predicate exp) env))
      (mceval (if-consequent exp) env)
      (mceval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (mceval (first-exp exps) env))
        (else
         (mceval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (mceval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (mceval (definition-value exp) env)
                    env)
  'ok)

;;; SECTION 4.1.2

(define (self-evaluating? exp)
  (or (number? exp)
      (string? exp)
      (char? exp)
      (boolean? exp)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (and (pair? exp) (eq? (car exp) tag)))

(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp)    (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp)       (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp)            (tagged-list? exp 'if))
(define (if-predicate exp)   (cadr exp))
(define (if-consequent exp)  (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp)     (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))

(define (last-exp? seq)  (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp)     (car exp))
(define (operands exp)     (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp)          (tagged-list? exp 'cond))
(define (and? exp)          (tagged-list? exp 'and))
(define (or? exp)          (tagged-list? exp 'or))
(define (cond-clauses exp)   (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause)   (cdr clause))

(define (expand-and operands)
  (cond [(null? operands) '#t]
        [(null? (cdr operands)) (car operands)]
        [else
         (let ([first (car operands)]
               [rest  (cdr operands)])
           (if first
                (expand-and rest)
                first))]))

(define (expand-or operands)
  (cond [(null? operands) '#f]
        [(null? (cdr operands)) (car operands)]
        [else
         (let ([first (car operands)]
               [rest  (cdr operands)])
           (if first
                first
                (expand-or rest)))]))


(define (and->if exp)
  (expand-and (cdr exp)))

(define (or->if exp)
  (expand-or (cdr exp)))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest  (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;; SECTION 4.1.3

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p)       (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env)           (car env))
(define the-empty-environment        '())

(define (make-frame variables values)
  (mcons variables (list->mlist values)))
(define (frame-variables frame)     (mcar frame))
(define (frame-values frame)        (mcdr frame))

(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (cons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))

(define (extend-environment vars vals base-env)
  (cond [(= (length vars) (length vals))
         (cons (make-frame vars vals) base-env)]
        [(< (length vars) (length vals))
         (error "Too many arguments supplied" vars vals)]
        [else
         (error "Too few arguments supplied" vars vals)]))

(define (lookup-variable-value var env)
  (let loop ([env env])
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let* ([frame (first-frame env)]
               [vars  (frame-variables frame)]
               [vals  (frame-values     frame)])
          (let find ([vars vars] [vals vals])
            (cond [(null? vars) (loop (cdr env))]
                  [(eq? var (car vars)) (mcar vals)]
                  [else (find (cdr vars) (mcdr vals))]))))))

(define (set-variable-value! var val env)
  (let loop ([env env])
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let* ([frame (first-frame env)]
               [vars  (frame-variables frame)]
               [vals  (frame-values     frame)])
          (let assign ([vars vars] [vals vals])
            (cond [(null? vars) (loop (cdr env))]
                  [(eq? var (car vars)) (set-mcar! vals val)]
                  [else (assign (cdr vars) (mcdr vals))]))))))

(define (define-variable! var val env)
  (let ([frame (first-frame env)])
    (let loop ([vars (frame-variables frame)] [vals (frame-values frame)])
      (cond [(null? vars) (add-binding-to-frame! var val frame)]
            [(eq? var (car vars)) (set-mcar! vals val)]
            [else (loop (cdr vars) (mcdr vals))]))))

;;; SECTION 4.1.4: Primitive procedures and initial environment

(define primitive-procedures
  (list (list 'car   car)
        (list 'cdr   cdr)
        (list 'cons  cons)
        (list 'null? null?)
        (list '+     +)
        (list '*     *)
        (list '-     -)
        (list '/     /)
        (list '<     <)
        (list '<=    <=)
        (list '=     =)
        (list '>=    >=)
        (list '>     >)
        (list 'error (lambda () (error "Metacircular Interpreter Aborted")))))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

;; let support (derived expression)
(define (let? exp)           (tagged-list? exp 'let))
(define (let-bindings exp)   (cadr exp))
(define (let-body exp)       (cddr exp))
(define (binding-vars bs)    (map car bs))
(define (binding-exprs bs)   (map cadr bs))

(define (let->application exp)
  (let* ([bs    (let-bindings exp)]
         [vars  (binding-vars bs)]
         [exprs (binding-exprs bs)]
         [body  (let-body exp)])
    (cons (cons 'lambda (cons vars body)) exprs)))

;; Printing and REPL
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define (top-mceval exp)
  (let ([val (mceval exp (setup-environment))])
    (user-print val)))

(define (eval . args)
  (error "Do not call eval"))

;; Setup environment once
(define (setup-environment)
  (let ([initial-env (extend-environment (primitive-procedure-names)
                                         (primitive-procedure-objects)
                                         the-empty-environment)])
    ;; Booleans and null
    (define-variable! 'true  true  initial-env)
    (define-variable! 'false false initial-env)
    (define-variable! 'null  '()    initial-env)
    ;; Define not as a regular procedure
    (define-variable! 'not
      (make-procedure '(x)
                      '((if x false true))
                      initial-env)
      initial-env)
    initial-env))

(define the-global-environment (setup-environment))
(define input-prompt "> ")

(define (driver-loop)
  (display input-prompt)
  (when (with-handlers
            ([exn:fail? (lambda (exn)
                          (display "Error: ")
                          (display (exn-message exn))
                          (newline)
                          #t)])
          (let ([input (read)])
            (if (eof-object? input)
                (begin
                  (newline)
                  #f)
                (let ([output (mceval input the-global-environment)])
                  (user-print output)
                  (newline)
                  #t))))
    (driver-loop)))

(define (main . argv)
  (driver-loop))

(provide mceval
         setup-environment
         main)

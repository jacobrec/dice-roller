(define-module (jroller eval)
  #:use-module (jroller freq)
  #:use-module (srfi srfi-1)
  #:export (dice-eval))

(define (dice? expr)
  (and (number? (car expr)) (number? (cdr expr))))

(define (dice-eval-expr expr)
  (define op (second expr))
  (define v1 (first expr))
  (define v2 (third expr))

  (define operation
    (cond
     ((string= "+" op) freq-add)
     ((string= "-" op) freq-sub)
     ((string= "*" op) freq-mul)
     ((string= "/" op) freq-div)))

  (operation (dice-eval v1) (dice-eval v2)))

(define (dice-eval expr)
  (cond
   ((number? expr) (freq-from-num expr))
   ((dice? expr) (freq-from-die expr))
   (else (dice-eval-expr expr))))


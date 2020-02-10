(define-module (jroller freq)
  #:use-module (srfi srfi-1)
  #:export (freq-compress

            freq-from-die
            freq-from-num

            freq-add
            freq-mul
            freq-sub
            freq-div))

(define (freq-from-die die)
  (define count (car die))
  (define size  (cdr die))
  (reduce freq-add '()
   (map (λ (x) (map (λ (x) (cons x 1)) (iota size 1)))
        (iota count))))

(define (freq-from-num num)
  `((,num . 1)))

(define (freq-compress f)
 (define res '())
 (define (loop l)
  (unless (null? l)
    (let* ((p (car l))
           (v (assoc (car p) res)))
      (set! res
       (if v
            (assoc-set! res (car p) (+ (cdr v) (cdr p)))
            (assoc-set! res (car p) (cdr p)))))
    (loop (cdr l))))
 (loop f)
 (let ((denom (cdadr res)))
   (define (loop l)
      (unless (null? l)
        (set! denom (gcd denom (cdar l)))
        (loop (cdr l))))
   (loop res)
   (map (λ(x) (cons (car x) (/ (cdr x) denom))) res)))

(define (freq-binop f1 f2 op)
 (define res '())
 (map (λ (v1)
        (map (λ (v2)
               (set! res (acons (op (car v1) (car v2)) (* (cdr v1) (cdr v2)) res)))
             f2))
      f1)
 (freq-compress res))

(define (freq-add f1 f2)
  (freq-binop f1 f2 +))

(define (freq-mul f1 f2)
 (freq-binop f1 f2 *))

(define (freq-sub f1 f2)
  (freq-binop f1 f2 -))

(define (freq-div f1 f2)
  (freq-binop f1 f2 /))

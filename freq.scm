(define-module (jroller freq)
  #:use-module (srfi srfi-1)
  #:use-module (jlib lists)
  #:export (freq-compress

            freq-get-normalizer
            freq-get-most-prob
            freq-eval

            freq-from-die
            freq-from-num

            freq-add
            freq-mul
            freq-sub
            freq-div))

(define (get-die-options die)
  (if (null? (caddr die))
    (values "" 0)
    (values (caaddr die) (cadar (cddr die)))))

(define (freq-from-die die)
  (define count (caadr die))
  (define size  (cdadr die))
  (define-values (opt-type opt-size) (get-die-options die))
  (define tsize (if (or (string= "x" opt-type)
                        (string= "X" opt-type))
                    opt-size (- count opt-size)))
  (define op (if (or (string= "X" opt-type) (string= "k" opt-type)) > <))
  (if (= 0 opt-size)
   (reduce freq-add '()
    (map (λ (x) (map (λ (x) (cons x 1)) (iota size 1)))
         (iota count)))
   (freq-die-options count size tsize op)))

(define (freq-die-options count size drop-size op)
  (define res
   (map (λ (p)
         (define sorted (sort p (λ (a b) (op (car a) (car b)))))
         (define sp (drop sorted drop-size))
         (define prob (apply + (map cdr p)))
         ;(format #t "prob: ~A | ~A -> ~A~%" prob p sp)
         (map (λ (x) (cons (car x) (* (cdr x) prob))) sp))
    (apply cartisian-product
          (map (λ (x) (map (λ (x) (cons x 1)) (iota size 1))) (iota count)))))
  (freq-compress
   (map (λ (x) (apply freq-add-many (map (λ (y) (list y)) x))) res)))

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
 (let ((denom (cdar res)))
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
                (set! res
                  (acons (op (car v1) (car v2))
                         (* (cdr v1) (cdr v2)) res)))
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


(define (freq-add-many f1 . rest)
  (define (fam f1 . rest)
    (if (null? rest) f1
      (apply fam (freq-add f1 (car rest)) (cdr rest))))
  (car (apply fam f1 rest)))

(define (freq-get-normalizer freq)
  (define (normalizer acc fre)
    (if (null? fre) acc
       (normalizer (+ acc (cdar fre))
                   (cdr fre))))
  (normalizer 0 freq))

(define (freq-get-most-prob freq)
  (define (normalizer acc fre)
    (if (null? fre) acc
       (normalizer (max acc (cdar fre))
                   (cdr fre))))
  (normalizer 1 freq))

(define (freq-eval freq)
  (define m (freq-get-normalizer freq))
  (define r (random m))
  (define (loop r freq)
    (define val (car freq))
    (if (< r (cdr val))
      (car val)
      (loop (- r (cdr val)) (cdr freq))))
  (loop r freq))

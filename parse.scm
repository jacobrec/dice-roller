(define-module (jroller parse)
  #:use-module (jlib parse)
  #:use-module (srfi srfi-1)
  #:export (parse
            commands))

(define commands '(("display" . ("graph" "alist"))
                   ("title" . ("show" "hide"))
                   ("percents" . ("show" "hide"))
                   ("graph-width" . ("full" "relative"))))

(define (parse/die)
 (parse/apply
  (parse/and (parse/int) (parse/lit "d") (parse/int))
  (λ (p) (cons (car p) (caddr p)))))

(define (parse/parens)
  (parse/between (parse/lit "(") (parse/expr) (parse/lit ")")))

(define (parse/obj)
  (parse/or
    (parse/parens)
    (parse/die)
    (parse/float)))

(define (parse/binop lower self ops)
  (lambda (str)
    ((apply parse/or
            (map (λ (op)
                   (parse/and lower (parse/lit op) self))
                 ops))
     str)))

(define-macro (parse/binary-level lower self ops)
 `(lambda (str)
    ((parse/or
       (parse/binop ,lower ,self ,ops)
       ,lower)
     str)))

(define (parse/mul) (parse/binary-level (parse/obj) (parse/mul) '("*" "/")))
(define (parse/add) (parse/binary-level (parse/mul) (parse/add) '("+" "-")))

(define (parse/expr) (parse/add))


(define (parse/optionset name values)
  (parse/and
   (parse/lit name)
   (parse/lit "=")
   (apply parse/or_lit values)))

(define (parse/cmd)
  (parse/apply
    (apply parse/or
           (map (λ(v)
                  (parse/optionset (car v) (cdr v)))
                commands))
    (λ (parsed)
     (list "cmd" (first parsed) (third parsed)))))

(define (parse)
  (parse/or
    (parse/cmd)
    (parse/expr)))

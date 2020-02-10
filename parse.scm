(define-module (jroller parse)
  #:use-module (jlib parse)
  #:export (parse))


(define (parse/die)
 ;(format #t "func: parse/die~%")
 (parse/apply
   (parse/and (parse/int) (parse/lit "d") (parse/int))
   (λ (p) (cons (car p) (caddr p)))))

(define (parse/obj)
  ;(format #t "func: parse/obj~%")
  (parse/or
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

(define (parse/mul)
  (parse/binary-level (parse/obj) (parse/mul) '("*" "/")))

(define (parse/add)
  (parse/binary-level (parse/mul) (parse/add) '("+" "-")))

(define (parse)
  (parse/add))

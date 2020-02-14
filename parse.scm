(define-module (jroller parse)
  #:use-module (jlib parse)
  #:use-module (srfi srfi-1)
  #:export (parse
            commands))

(define commands '(("display"  . ("graph" "relative" "atleast" "atmost" "alist"))
                   ("title"    . ("show" "hide"))
                   ("mode"     . ("chance" "result"))
                   ("percents" . ("show" "hide"))))

(define statements '("help" "quit" "exit" "options"))

(define (parse/die)
 (parse/apply
  (parse/and (parse/int) (parse/lit "d") (parse/int))
  (λ (p)
    (if (and (> (car p) 0) (> (caddr p) 0))
      (cons (car p) (caddr p))
      'parse-error))))

(define (parse/die-ops)
  (define parser
    (parse/and
      (parse/die)
      (parse/? (parse/and
                (parse/or_lit "k" "K" "x" "X")
                (parse/int)))))
  (parse/apply
   parser
   (λ (x)
     (cond
      ((null? (cadr x)) (cons 'die x))
      ((and (not (= 0 (cadadr x)))
            (> (caar x) (cadadr x))) (cons 'die x))
      (else 'parse-error)))))

(define (parse/parens)
 (parse/between (parse/lit "(") (parse/expr) (parse/lit ")")))

(define (parse/obj)
  (parse/or
    (parse/parens)
    (parse/die-ops)
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
     (list 'cmd (first parsed) (third parsed)))))

(define (parse/stmt)
  (parse/apply
    (apply parse/or_lit statements)
    (λ (val) (list 'stmt val))))

(define (parse/exprstmt)
  (parse/apply
    (parse/expr)
    (λ (val) (list 'expr val))))

(define (parse/val)
  (parse/or (parse/cmd) (parse/stmt) (parse/exprstmt)))

(define (parse)
  (λ (str)
    ((parse/or
       (parse/apply
         (parse/and (parse/val) (parse/lit ";") (parse))
         (λ (x) (cons (first x) (third x))))
       (parse/apply
         (parse/and (parse/val) (parse/? (parse/lit ";")))
         (λ (x) (list (car x)))))
     str)))

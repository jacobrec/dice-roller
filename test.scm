(use-modules (guest test))
(use-modules (ice-9 receive))

(define parse (@ (jroller parse) parse))
(define parse/expr (@@ (jroller parse) parse/expr))
(define parse/die (@@ (jroller parse) parse/die))
(define parse/die-ops (@@ (jroller parse) parse/die-ops))


(define-suite (jroller)
  (define-suite (parse)
    (define-test (die)
      (assert-equal? '(3 . 6)
       ((parse/die) "3d6"))
      (assert-equal? 'parse-error
       ((parse/die) "3d0"))
      (assert-equal? 'parse-error
            ((parse/die) "0d3")

         (define-test (die-ops)
           (assert-equal? '(#:die (3 . 6) ("k" 2))
            ((parse/die-ops) "3d6k2"))
           (assert-equal? '(#:die (3 . 6) ("x" 2))
            ((parse/die-ops) "3d6x2"
             (assert-equal? '(#:die (3 . 6) ("K" 2))
              ((parse/die-ops) "3d6K2"))
             (assert-equal? '(#:die (3 . 6) ("X" 2))
              ((parse/die-ops) "3d6X2"))
             (assert-equal? 'parse-error
              ((parse/die-ops) "3d6k0"))
             (assert-equal? 'parse-error))
            ((parse/die-ops) "3d6k3"
             (assert-equal? 'parse-error))
            ((parse/die-ops) "3d6k4")))

         (define-test (parse-expr)
           (assert-equal? '(1.0 "*" 3.0)
                          ((parse/expr) " 1 * 3"))

           (assert-equal? '((#:die (1 . 6) ()) "+" (#:die (1 . 6) ()))
                          ((parse/expr) "1d6 + 1d6"))

           (assert-equal? 'parse-error
                          ((parse/expr) "(1 + 3"))

           (assert-equal? ")"
                          (receive (parsed res) ((parse/expr) "1 + 3)"))
                        res))

         (define-test (parse-command)))
      (assert-equal? '((#:stmt "help"
                        ((parse) "help")))

       (define-test (parse-option)))
      (assert-equal? '((#:cmd "mode" "result"
                        ((parse) "mode=result")))

       (define-test (parse)))
      (assert-equal? 'parse-error
                       ((parse) "aaahhh"))
      (assert-equal? '((#:expr 1.0))
                     ((parse) "1")))))

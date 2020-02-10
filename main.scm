(use-modules (jroller parse))
(use-modules (jroller eval))
(use-modules (jroller printer))

(use-modules (srfi srfi-1))
(use-modules (ice-9 readline))
(use-modules (ice-9 rdelim))

#;
(define options '(("display" . "graph")
                  ("title" . "show")))
(define options ((λ (commands)
                   (define (inner cmds)
                     (if (null? cmds) '()
                      (acons (caar cmds) (cadar cmds) (inner (cdr cmds)))))
                   (inner commands))
                 commands))

(define (do-expr str parsed)
 (display-freq options str (dice-eval parsed)))

(define (do-option input)
  (define cmd (first input))
  (define val (second input))
  (set! options
    (assoc-set! options cmd val)))

(define (do-error str)
  (format #t "Syntax error: ~A~%" str))

(define (do-help)
  (format #t "Enter a Command or an Expression~%")
  (format #t "================================~%")
  (format #t "Expressions:~%")
  (format #t "  Ex) 2d6~%")
  (format #t "  Ex) 1d4 + (2 * 3)~%")
  (format #t "  Ex) (1d3 * 2d7) + 1d2~%")
  (format #t "  Ex) 1~%")

  (format #t "~%~%Commands:~%")
  (format #t "  Ex) display=graph~%~%")
  (format #t "All Commands:~%")
  (map (λ(x)
         (format #t "  ~a={~a}~%" (car x) (string-join (cdr x) ",")))
       commands))

(define (handle-input input)
  (define parsed ((parse) input))
  (cond
   ((string= "help" input) (do-help))
   ((string= "options" input) (format #t "~A~%" options))
   ((eq? 'parse-error parsed) (do-error input))
   ((and (pair? parsed) (string? (car parsed)) (string= "cmd" (car parsed)))
    (do-option (cdr parsed)))
   (else (do-expr input parsed))))

(define (main)
  (define input (readline "> "))
  (unless (or (eof-object? input) (string= "exit" input) (string= "quit" input))
    (handle-input input)
    (main)))

(main)

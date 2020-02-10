(use-modules (jroller parse))
(use-modules (jroller eval))
(use-modules (jroller printer))

(use-modules (srfi srfi-1))
(use-modules (ice-9 readline))
(use-modules (ice-9 rdelim))

#;
(define options '(("display" . "graph")
                  ("title" . "show")))
(define options (acons "display" "graph"
                  (acons "title" "show"
                    (acons "percents" "show"
                      (acons "graph-width" "full" '())))))

(define (parse-eval-print str parsed)
 (display-freq options str (dice-eval parsed)))

(define (do-cmd input)
  (define cmd (first input))
  (define val (second input))
  (set! options
    (assoc-set! options cmd val)))

(define (do-error str)
  (format #t "Syntax error: ~A~%" str))

(define (do-input input)
  (define parsed ((parse) input))
  (if (eq? 'parse-error parsed)
    (do-error input)
    (if (and (pair? parsed) (string? (car parsed)) (string= "cmd" (car parsed)))
      (do-cmd (cdr parsed))
      (parse-eval-print input parsed))))

(define (main)
  (define input (readline "> "))
  (unless (or (eof-object? input) (string= "exit" input) (string= "quit" input))
    (do-input input)
    (main)))

(main)

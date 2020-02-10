(define-module (jroller printer)
  #:use-module (ice-9 pretty-print)
  #:use-module (jroller freq)
  #:export (display-freq))

(define (get-option op options)
  (cdr (assoc op options)))

(define (is-option op val options)
  (string= val (get-option op options)))

(define (display-freq options str freq)
  (when (is-option "title" "show" options)
    (hash-title str))
  (let ((gt (get-option "display" options)))
    (cond
     ((is-option "display" "alist" options) (display-freq-pretty-print str freq))
     ((is-option "display" "graph" options) (display-freq-graph
                                             options str freq 80)))))

(define (hash-title str)
  (display "### ")
  (display str)
  (display " ###\n"))

(define (display-freq-pretty-print str freq)
  (pretty-print freq)
  (display "\n"))


(define (display-freq-graph options str freq max-width)
  (define label-width 5)
  (define width (* 8 max-width))
  (define n ((if (is-option "graph-width" "full" options)
                 freq-get-most-prob freq-get-normalizer) freq))
  (define freq2 (sort freq (λ (a b) (< (car a) (car b)))))
  (define (int num)
    (inexact->exact (truncate num)))

  (map (λ(x)
         (define tiles (int (round (/ (* (cdr x) width) n))))
         (define fulls (int (/ tiles 8)))
         (define partial-type (- tiles (* 8 fulls)))
         (define header (number->string (car x)))
         (display header)
         (display (make-string (- label-width (string-length header)) #\space))
         (display (make-string fulls #\█))
         (display
           (case partial-type
              ((0) #\space)
              ((1) #\▏)
              ((2) #\▎)
              ((3) #\▍)
              ((4) #\▌)
              ((5) #\▋)
              ((6) #\▊)
              ((7) #\▉)))
         (display "\n"))
       freq2))


(define-module (jroller printer)
  #:use-module (ice-9 pretty-print)
  #:use-module (jroller freq)
  #:export (display-freq
            is-option))

(define (get-option op options)
  (cdr (assoc op options)))

(define (is-option op val options)
  (string= val (get-option op options)))

(define (display-freq options str freq)
  (when (is-option "title" "show" options)
    (hash-title str))
  (if (is-option "mode" "chance" options)
    (let ((gt (get-option "display" options)))
      (cond
       ((is-option "display" "alist" options) (display-freq-pretty-print freq))
       ((is-option "display" "graph" options) (display-freq-graph
                                               options freq 70))))
    (display-evaled-freq freq)))

(define (hash-title str)
  (display "### ")
  (display str)
  (display " ###\n"))

(define (display-evaled-freq freq)
  (format #t " => ~A~%" (freq-eval freq)))

(define (display-freq-pretty-print freq)
  (pretty-print freq)
  (display "\n"))


(define (display-freq-graph options freq max-width)
  (define max-digits 4)
  (define width (* 8 max-width))
  (define denom (freq-get-normalizer freq))
  (define n ((if (is-option "graph-width" "full" options)
                 freq-get-most-prob freq-get-normalizer) freq))
  (define freq2 (sort freq (λ (a b) (< (car a) (car b)))))
  (define (int num)
    (inexact->exact (truncate num)))
  (define is-percent (is-option "percents" "show" options))
  (define label-width (if is-percent 12 (+ 1 max-digits)))

  (map (λ(x)
         (define percent (exact->inexact
                          (/ (int (* 10000 (exact->inexact (/ (cdr x) denom)))) 100)))
         (define tiles (int (round (/ (* (cdr x) width) n))))
         (define fulls (int (/ tiles 8)))
         (define partial-type (- tiles (* 8 fulls)))
         (define header (number->string (car x)))
         (define pheader (if is-percent (number->string percent) ""))
         (if is-percent
           (format #t "~A~A- ~A%" header
                   (make-string (- max-digits (string-length header)) #\space)
                   pheader)
           (format #t "~A~A" header
                   (make-string (- max-digits (string-length header)) #\space)))

         (display (make-string (- label-width
                                 (+ max-digits (string-length pheader)))
                              #\space))
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

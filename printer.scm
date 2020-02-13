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
       ((or (is-option "display" "graph" options)
            (is-option "display" "atleast" options)
            (is-option "display" "atmost" options))
        (display-freq-graph options freq 70))))
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

(define (int num)
  (inexact->exact (truncate num)))

(define (display-graph-item-header is-percent x percent)
  (define label-width (if is-percent 12 (+ 1 max-digits)))
  (define max-digits 4)
  (define header (number->string (car x)))
  (define pheader (if is-percent (number->string percent) ""))
  (if is-percent
       (format #t "~A~A-> ~A%" header
                  (make-string (- max-digits (string-length header)) #\space)
              pheader)
       (format #t "~A~A" header
                  (make-string (- max-digits (string-length header)) #\space)))

  (display (make-string (- label-width
                            (+ max-digits (string-length pheader)))
                        #\space)))

(define (display-tiles tiles)
  (define fulls (int (/ tiles 8)))
  (define partial-type (- tiles (* 8 fulls)))
  (define chars #(#\space #\▏ #\▎ #\▍ #\▌ #\▋ #\▊ #\▉ #\█))
  (display (make-string fulls (vector-ref chars 8)))
  (display (vector-ref chars partial-type))
  (display "\n"))

(define (display-freq-graph options freq max-width)
  (define width (* 8 max-width))
  (define denom (freq-get-normalizer freq))
  (define n ((if (and (is-option "display" "graph" options))
                 freq-get-most-prob freq-get-normalizer) freq))
  (define is-percent (is-option "percents" "show" options))
  (define cumnum 0)
  (define (get-percent val denom)
    (define sig 100)
    (exact->inexact
      (/ (int (* 100 sig
                 (exact->inexact
                  (/ val denom)))) 100)))

  (map (λ (x)
         (set! cumnum (+ cumnum (cdr x)))
         (let* ((d-width (cond ((is-option "display" "atmost" options) cumnum)
                               ((is-option "display" "atleast" options)
                                (+ 1 (- denom cumnum)))
                               (else (cdr x))))
                (tiles (int (round (/ (* d-width width) n))))
                (percent (get-percent d-width denom)))
           (display-graph-item-header is-percent x percent)
           (display-tiles tiles)))
       (sort freq (λ (a b) (< (car a) (car b))))))

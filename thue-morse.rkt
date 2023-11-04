#lang racket/base

(define (complement digits)
  (define (invert d)
    (if (zero? d) 1 0 ))
  (map invert digits))

(define (thue-morse n)
  (define (loop tms n)
    ;;exit condition
    (if (zero? n) 
        tms
        (loop (append tms (complement tms)) (- n 1))))
    ;;start the loop
    (loop '(0) n))

(time (length (thue-morse 20)))
        
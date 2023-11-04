#lang racket

(define (lsco target)
  (define (loop n)
    (let ((m (quotient target n)))
      (cond 
        [(positive? (modulo target n))
         (loop (- n 1))]
        [(= (modulo n 2) (modulo m 2))
         (values (- m n -1) (+ m n -1) n)]
        [else 
         (loop (- n 1))])))
  (loop (integer-sqrt target)))
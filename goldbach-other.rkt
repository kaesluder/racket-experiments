#lang racket/base
(require math/number-theory)

(define (goldbach-other-exception? n)
  (if (prime? n) #f
      (not (for/or ([i (in-range 1 (+ 1 (integer-sqrt (quotient n 2)))) ])
             (prime? (- n (* 2 i i)))))))

(define (search-goldbach n)
  (define (iter i)
    (cond
      [(> i n) #f] ;out of range exit
      [(goldbach-other-exception? i) i] 
      [else (iter (+ i 2))]))
  (iter 3))
      
  
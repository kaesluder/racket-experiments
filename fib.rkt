#lang racket/base
(define (fib n)
  (let loop ((a 0) (b 1) (n n))
    (if (<= n 0) a
        (loop b (+ a b) (- n 1)))))


;;;slow
(define (fib-rec n)
  (if (< n 2)
      n
      (+ (fib-rec (- n 1))
         (fib-rec (- n 2)))))


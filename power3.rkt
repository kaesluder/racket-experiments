#lang racket

(define (power3? n)
  (if (= n 1)
      #t
      (call-with-values 
       (lambda () (quotient/remainder n 3))
       (lambda (q r)
         (if (> r 0)
             #f
             (power3? q))))))
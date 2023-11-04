#lang racket

(define (homework-1 n [nsum 0])
  (if (= n 0) 
      nsum
      (homework-1 (- n 1) (+ nsum (+ n n -1)))))

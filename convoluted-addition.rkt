#lang racket
(define (+ a b) (if (= a 0) b (add1 (+ (sub1 a) b))))


(define (myadd a b) ; Define myadd     
  (if (= a 0)    
      b    
      (myadd (sub1 a) (add1 b)))) ; Call myadd    
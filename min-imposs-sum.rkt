#lang racket

(define (sum-of? num list)
  (define (less-than-num? x)
    (< x num))
  (if (member num list) 
      #t
      (let ([list2 (filter less-than-num? list)])
        (for/or ([i list2])
          (sum-of? (- num i) (remove i list2))))))

(define (min-impossible-sum list)
  (define (loop n)
    (if (not (sum-of? n list))
        n
        (loop (+ 1 n))))
  (loop 1))

(define (min-impossible-sum2 xs)
  (let loop ((xs (sort xs <)) (x 1))
    (if (or (null? xs) (< x (car xs))) x
      (loop (cdr xs) (+ x (car xs))))))
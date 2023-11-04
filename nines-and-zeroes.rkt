#lang racket
(define (digits n . args)
  (let ((b (if (null? args) 10 (car args))))
    (let loop ((n n) (d '()))
      (if (zero? n) d
          (loop (quotient n b)
                (cons (modulo n b) d))))))

(define (undigits ds . args)
  (let ((b (if (null? args) 10 (car args))))
    (let loop ((ds ds) (n 0))
      (if (null? ds) n
          (loop (cdr ds) (+ (* n b) (car ds)))))))

(define (f n)
  (define (nine x) (if (= x 1) 9 x))
  (let loop ((i 1))
    (let ((x (undigits (map nine (digits i 2)))))
      (if (zero? (modulo x n)) x (loop (+ i 1))))))

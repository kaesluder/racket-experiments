#lang racket
(require math)
(require file/sha1)

(define lc-rng-a 69069)
(define lc-rng-c 1234567)
(define lc-rng-m (expt 2 32))

(define (next x) 
  (modulo (+ (* lc-rng-a x) lc-rng-c) lc-rng-m))

(define (prev x)
  (modulo (* (modular-inverse lc-rng-a lc-rng-m) (- x lc-rng-c)) lc-rng-m))

(define (test-prev-next-1)
  (for/and ([a (list 2718281828 1234567 98757673 0)])
    (= a (prev (next a)))))

(define (test-prev-next-2)
  (define (loop n x)
    (if (= n 0)
        x
        (begin 
         (display x)
         (newline)
         (loop (- n 1) (next x)))))
  (loop 1000 0))

(define (number->bytes x (byte-string (list)))
  (if (= x 0)
      (apply bytes byte-string)
      (number->bytes (quotient x (expt 2 8)) (cons (modulo x (expt 2 8)) byte-string))))
  





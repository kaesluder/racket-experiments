#lang racket
(require math/number-theory)
(require (except-in racket/list permutations))
(define (knap target xs)
  (if (null? xs) 0
    (if (< target (car xs))
        (knap target (cdr xs))
        (max (+ (car xs) (knap (- target (car xs)) (cdr xs)))
             (knap target (cdr xs))))))

(define (knap2 target xs)
  (if (null? xs) 1
    (if (< target (car xs))
        (knap2 target (cdr xs))
        (max (* (car xs) (knap2 (quotient target (car xs)) (cdr xs)))
             (knap2 target (cdr xs))))))

(define (factors n)
  (apply append (map (lambda (li) (make-list (second li) (first li))) (factorize n))))
  

(define (nsd n)
  (let* ((fs (map log (reverse (factors n))))
         (b (inexact->exact (round (exp
              (knap (/ (apply + fs) 2) fs)))))
         (a (/ n b)))
    (list a b)))

(define (nsd2 n)
  (let* ((fs (reverse (factors n)))
         (b (knap2 (integer-root n 2) fs))
         (a (/ n b)))
    (list a b)))
#lang racket
(require math/number-theory)

(define (fermat-1? n)
  (exact? (sqrt (apply + (divisors (* n n n))))))

(define (list-fermat-1 limit)
  (filter fermat-1? (range 1 (+ 1 limit))))

(define (fermat-1-partner n)
  (sqrt (apply + (divisors (* n n n)))))



(define (fermat-2? n)
  (let-values ([(croot remainder) 
                (integer-root/remainder (apply + (divisors (* n n))) 3)])
    (= 0 remainder)))

(define (fermat-2-partner n)
  (integer-root (apply + (divisors (* n n))) 3))

(define (list-fermat-2 limit)
  (map (lambda (x) (cons x (fermat-2-partner x)))
       (filter fermat-2? (range 1 (+ 1 limit)))))
    






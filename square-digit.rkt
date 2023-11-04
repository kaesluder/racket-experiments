#lang racket

(define (gen-permutations)
  (for*/list ([a (range 10)]
	      [b (range a 10)]
	      [c (range b 10)]
	      [d (range c 10)]
	      [e (range d 10)]
	      [f (range e 10)]
	      [g (range f 10)])
    (list a b c d e f g)))
	      

(define (int-to-digits n)
  (define (iter x li)
    (if (< x 10)
        (cons x li)
        (iter (quotient x 10) (cons (modulo x 10) li))))
  (iter n (list)))


(define (digits-to-int li)
  (define (iter li result)
    (if (empty? li)
        result
        (iter (cdr li) (+ (* 10 result) (car li)))))
  (iter li 0))

(define (square-digit-sum digits)
  (apply + (map (lambda (x) (* x x)) digits)))

(define (square-digit-sum-terminus n)
  (define (iter x)
    (if (member x (list 0 1 89))
        x
        (iter (square-digit-sum (int-to-digits x)))))
  (iter n))

(define (count-89-sums li (result 0))
  (if (empty? li)
      result
      (if (= 89 (square-digit-sum-terminus (digits-to-int (car li))))
          (count-89-sums (cdr li) (+ 1 result))
          (count-89-sums (cdr li) result))))


(time (count-89-sums (gen-permutations)))










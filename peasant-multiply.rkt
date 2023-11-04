#lang racket

(define (peasant-multiply left right)
  (define (pm-loop left right sum)
    (if (<= left 0)
        sum
        (if (odd? left)
            (pm-loop (quotient left 2) (+ right right) (+ sum right))
            (pm-loop (quotient left 2) (+ right right) sum))))
  (pm-loop left right 0))

(define (bab-sqrt n)
  (cond [(< n 1) (/ (bab-sqrt (* n 4)) 2)]
        [(<= 4 n) (* (bab-sqrt (/ n 4)) 2)]
        [else 
         (for/fold 
             ([x (/ (+ n 1) 2)])
           ([i (range 5)])
           (/ (+ x (/ n x)) 2))]))

(define (bab-sqrt2 n)
  (define (iter this last)
    (if (< (abs (- this last)) .001)
        this
        (iter (/ (+ this (/ n this)) 2) this)))
  (iter (/ (+ (exact->inexact n) 1) 2) 
        ;;coerce to inexact since we're approximating anyway
        ;;racket seems to grind on the rationals produced by
        ;;very large numbers.
        0)) 






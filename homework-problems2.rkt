#lang racket

(require math/number-theory)

(define (homework4 xs [results (list)] [st (set)])
  (cond 
    [(empty? xs) (reverse results)]
    [(set-member? st (car xs))
      (homework4 (cdr xs) results st)]
    [else
     (homework4 (cdr xs) (cons (car xs) results) (set-add st (car xs)))]))

(define (homework5 xs ys [results (list)]) 
  ;;find elements that exist in both lists. 
  (cond [(or (empty? xs) (empty? ys)) ;;exit case if either list is empty, there are no more matches.
         (reverse results)]
        ;;if the number at the top of the lists are equal
        ;;add it to results, and move on down both lists.
        [(= (car xs) (car ys))
         (homework5 (cdr xs) (cdr ys) (cons (car xs) results))]
        
        ;;if the number on top of xs is less than 
        ;;ys, iterate xs
        [(< (car xs) (car ys))
         (homework5 (cdr xs) ys results)]
        
        ;;otherwise iterate ys.
        [else
         (homework5 xs (cdr ys) results)]))


(define (is-cube-root? n)
  (let ((x (integer-root n 3)))
    (= n (* x x x ))))

(define (iroot k n)
  (let ((k-1 (- k 1)))
    (let loop ((u n) (s (+ n 1)))
      (if (<= s u) s
        (loop (quotient (+ (* k-1 u) (quotient n (expt u k-1))) k) u)))))

(define (babylon-cube a xn)
  (quotient (+ (quotient a (* xn xn)) (* 2 xn)) 3))

(define (delta-x A n (x (/ A 3)))
    (* (/ 1 n) (- (/ A (expt x (- n 1))) x)))

(define (nth-root A n (precision .01))
  (define (delta-x x)
    (* (/ 1.0 n) (- (/ A (expt x (- n 1))) x)))
  (define (loop x)
    (display (* 1.0 x))
    (display "\n")
    (let ((dx (delta-x x)))
      (if (<= (abs dx) precision)
          x
          (loop (+ x dx)))))
  (loop (/ A n)))

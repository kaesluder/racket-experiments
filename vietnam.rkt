#lang racket/base
(require racket/list)
(require racket/generator)

(define (vietnam a b c d e f g h i)
  (+ a
     (/ (* 13 b) c)
     d
     (* 12 e)
     (* f -1)
     -11
     (/ (* g h) i)
     -10))

(define perm-heap 
  (generator 
   ()
   (define (heap n xs)
     (if (= n 1)
         (yield xs)
         (for ([i (range n)])
           (heap (- n 1) xs)
           (if (odd? n)
               (let ([tmp (vector-ref xs 0)])
                 (vector-set! xs 0 (vector-ref xs (- n 1)))
                 (vector-set! xs (- n 1) tmp))
               (let ([tmp (vector-ref xs i)])
                 (vector-set! xs i (vector-ref xs (- n 1)))
                 (vector-set! xs (- n 1) tmp))))))
   (heap 9 (vector 1 2 3 4 5 6 7 8 9)))) 
             
             

(define (vietnam? xs)
  (= 66 (apply vietnam xs)))

(define (vietnam2? xs)
  (let ((v (apply vietnam xs)))
    (and (> v 66) (< v 67))))

(define (fractional-terms)
  (define (foo a b c d e f g h i)
    (+ (/ (* 13 a) b) (/ (* c d) e)))
  (remove-duplicates 
   (for/list ([xs (permutations '(1 2 3 4 5 6 7 8 9))]
              #:when (integer? (apply foo xs)))
    (list (/ (* 13 (list-ref xs 0)) (list-ref xs 1)) 
          (/ (* (list-ref xs 2) (list-ref xs 3))
             (list-ref xs 4)))) equal?))

(time (length (filter vietnam? (permutations '(1 2 3 4 5 6 7 8 9)))))


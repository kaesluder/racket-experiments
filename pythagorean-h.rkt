#lang racket
(require racket/set)

(define (hypotenuse m n k)
  (* k (+ (* m m) (* n n))))

(define (a m n k)
  (* k (- (* m m) (* n n))))

(define (all-hypotenuse limit)
  (let ([results (mutable-set)])
    (for ((m (range 2 (add1 (integer-sqrt (- limit 1))))))
      (for ((n (range 1 (min m (add1 (integer-sqrt (- limit (* m m))))))))
        (for ((k (range 1 (/ limit (hypotenuse m n 1)))))
          (set-add! results (hypotenuse m n k)))))
    (set->list results)))
      
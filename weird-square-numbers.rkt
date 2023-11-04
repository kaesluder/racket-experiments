#lang racket

(define natural-log-10 (log 10))

(define (significant-digits n)
  (ceiling (/ (log n) natural-log-10)))

(define (weird-square? n)
  (= n (modulo (* n n) (expt 10 (significant-digits n))))) 


(define (weird-square-sum limit)
  (for/sum ([i (in-range 1 (expt 10 limit))])
    (if (weird-square? i)
        i
        0)))

(define (ilog b n)
  (let loop1 ((lo 0) (b^lo 1) (hi 1) (b^hi b))
    (if (< b^hi n) (loop1 hi b^hi (* hi 2) (* b^hi b^hi))
      (let loop2 ((lo lo) (b^lo b^lo) (hi hi) (b^hi b^hi))
        (if (<= (- hi lo) 1) (if (= b^hi n) hi lo)
          (let* ((mid (quotient (+ lo hi) 2))
                 (b^mid (* b^lo (expt b (- mid lo)))))
            (cond ((< n b^mid) (loop2 lo b^lo mid b^mid))
                  ((< b^mid n) (loop2 mid b^mid hi b^hi))
                  (else mid))))))))

(define (expm b e m)
  (define (m* x y) (modulo (* x y) m))
  (cond ((zero? e) 1)
        ((even? e) (expm (m* b b) (/ e 2) m))
        (else (m* b (expm (m* b b) (/ (- e 1) 2) m)))))

(define (curious n)
  (let ((xs (list 1)))
    (do ((i 1 (+ i 1))) ((= i n) (apply + xs))
      (let* ((x (expm 5 (expt 2 i) (expt 10 i)))
             (y (- (expt 10 i) x -1)))
        (display i) (display ": ")
        (display x) (display " ")
        (display y) (newline)
        (when (not (member x xs))
          (set! xs (cons x xs)))
        (when (not (member y xs))
          (set! xs (cons y xs)))))))
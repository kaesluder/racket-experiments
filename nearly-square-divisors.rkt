#lang racket
(require math/number-theory)
(define (factors n)
  (if (even? n) (cons 2 (factors (/ n 2)))
    (let loop ((n n) (f 3) (fs '()))
      (cond ((< n (* f f)) (reverse (cons n fs)))
            ((zero? (modulo n f)) (loop (/ n f) f (cons f fs)))
            (else (loop n (+ f 2) fs))))))

(define (powerset aL)
  (if (empty? aL)
      '(())
      (let ((rst (powerset (rest aL))))
        (append (map (lambda (x) (cons (first aL) x))
                     rst)
                rst))))


(define (divisors2 n) ;;not needed when using math/number-theory
  (sort (remove-duplicates (map (lambda (li) (apply * li)) (powerset (factors n)))) < ))

(define (nearly-square-divisors n)
  (cond
    [(prime? n) (values 1 n)] ;;check if prime
    [(zero? (modulo n (integer-sqrt n))) ;;check if square number
     (values (integer-sqrt n) (integer-sqrt n))]
    [else ;use the middle two values
     (let* ([divs (divisors n)]
            [len (length divs)])
       (values (list-ref divs (sub1 (/ len 2)))
               (list-ref divs (/ len 2))))]))




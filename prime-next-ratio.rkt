#lang racket
(require math/number-theory)

(define (run-primes2 max (prev 7) (results (make-hash)))
  (if (> prev max)
      results
      (let* ((prev-10 (modulo prev 10))
             (next (next-prime prev))
             (next-10 (modulo next 10))
             (key (+ (* prev-10 10) next-10)))
        (hash-update! results key add1 0)
        (run-primes max next results))))

(define (compose-key a b)
  (+ (* 10 a) b))

(define (sieve n)
  (define primes (make-vector (add1 n) #t))
  (for* ([i (in-range 2 (add1 n))]
         #:when (vector-ref primes i)
         [j (in-range (* i i) (add1 n) i)])
    (vector-set! primes j #f))
  (for/list ([n (in-range 2 (add1 n))]
             #:when (vector-ref primes n))
    n))

(define (prime-percentages n phash)
  (define keys (map (lambda (x) (compose-key n x)) (list 1 3 7 9)))
  (define vals (map (lambda (x) (hash-ref phash x)) keys))
  (define total (apply + vals))
  (for ((i keys)
        (j vals))
    (printf "~a ~.a~n" i (/ j total 1.00))))

(define (run-prime-percentages n)
  (define primes (run-primes n))
  (for ((i (list 1 3 7 9)))
    (printf "~n")
    (prime-percentages i primes)))


(define (run-primes n)
  (define primes (drop (sieve n) 3))
  (define results (make-hash))
  (define (iter prev li)
    (if (null? li)
        results
        (let ((next (car li)))
          (hash-update! 
           results
           (+ (* 10 (modulo prev 10)) (modulo next 10))
           add1
           0)
          (iter next (cdr li)))))
  (iter (car primes) (cdr primes)))
            
    
  

  

  
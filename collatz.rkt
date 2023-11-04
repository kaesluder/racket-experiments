#lang racket
(require math/number-theory)

(define (collatz start)
  (define (loop curr li)
    (cond
      [(= 1 curr) (cons curr li)]
      [(even? curr) (loop (/ curr 2) (cons curr li))]
      [else (loop (+ 1 (* 3 curr)) (cons curr li))]))
  (loop start (list)))


(define (collatz-next curr)
  (if (even? curr)
      (/ curr 2)
      (+ 1 (* 3 curr))))

(define (collatz-primes start target)
  (define (loop curr prime-count)
    (cond [(= prime-count target) start]
          [(= 1 curr) #f]
          [(prime? curr)
           (loop (collatz-next curr) (+ 1 prime-count))]
          [else
           (loop (collatz-next curr) prime-count)]))
  (loop start 0))

(define (collatz-primes-memo start result-hash)
  (define (loop curr prime-count)
    (cond 
      [(= 1 curr) prime-count]
      [(hash-ref result-hash curr #f) 
       (+ prime-count (hash-ref result-hash curr))]
      [(prime? curr) 
       (loop (collatz-next curr) (+ 1 prime-count))]
      [else
       (loop (collatz-next curr) prime-count)]))
  (loop start 0))

(define (collatz-primes-to-limit lim)
  (define (loop n result-hash)
    (define prime-count (collatz-primes-memo n result-hash))
    (if (>= prime-count lim)
        n
        (loop (+ 1 n) (hash-set result-hash n prime-count))))
  (loop 2 (hash)))

(collatz-primes-to-limit 65)

    
    



#lang racket
(require math/number-theory)

(define (aliquot-divisors x)
  (drop-right (divisors x) 1))

(define (sum-aliquot x)
  (foldl + 0 (aliquot-divisors x)))

(define (multiply-perfect-numbers limit)
  (for/list ([x (range 1 (+ limit 1))]
             #:when (= 0 (modulo (sum-aliquot x) x)))
    x))

(define (sum-sieve limit)
  (let ((sieve (make-vector (+ limit 1) 1)))
    (for* ([i (range 2 limit)]
          [j (range (+ i i) (+ limit 1) i)])
      (vector-set! sieve j (+ (vector-ref sieve j) i)))
    sieve))

(define (multiply-perfect-numbers2 limit)
  (define sieve (sum-sieve limit))
  (for/list ([i (range 1 (add1 limit))]
             #:when (= 0 (modulo (vector-ref sieve i) i)))
    i))


(define (char-histogram str)
  (define histogram (make-hash))
  (for ([c (string->list str)])
    (hash-set! histogram c 
               (add1 (hash-ref histogram c 0))))
  histogram)

(define (can-make-palendrome str)
  (define vals (hash-values (char-histogram str)))
  (if (< 1 (length (filter odd? vals)))
      #f
      #t))

(define (build-palendrome str)
  (define hist (char-histogram str))
  (define first 
    (for*/list ([k (hash-keys hist)]
                #:when (even? (hash-ref hist k))
                [j (range (/ (hash-ref hist k) 2))])
      k))
  (define middle 
    (for/first ([k (hash-keys hist)]
                #:when (odd? (hash-ref hist k)))
      k))
  (list->string 
   (append first
           (if middle (list middle) (list))
           (reverse first))))

(define (build-palendrome-or-fail str)
  (if (can-make-palendrome str)
      (build-palendrome str)
      #f))
  
  
 
             

             



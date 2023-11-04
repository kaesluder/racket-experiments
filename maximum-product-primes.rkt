#lang racket

;;http://programmingpraxis.com/2015/08/28/maximum-product-of-two-primes-less-than-n/


(require data/bit-vector)

(define (sieve limit)
  (define bv (make-bit-vector (+ limit 1) #f))
  (bit-vector-set! bv 0 #t)
  (bit-vector-set! bv 1 #t)
  (for* ([i (in-range (add1 (sqrt limit)))] #:unless (bit-vector-ref bv i)
         [j (in-range (* 2 i) (bit-vector-length bv) i)])
    (bit-vector-set! bv j #t))
  ;; translate to a list of primes
  (for/list ([i (bit-vector-length bv)] #:unless (bit-vector-ref bv i)) i))



(define (max-prod-prime n)
  (define primes (sieve (quotient n 2)))
  (define rprimes (reverse primes))
  (define (iter ps rps maxa maxb)
    (let* ([a (car ps)]
           [helper-p (lambda (x) (> x (quotient n a)))]
           [nrps (dropf rps helper-p)]
           [b (car nrps)])
      (if (> a b)
          (values (* maxa maxb) maxa maxb)
          (if (> (* a b) (* maxa maxb))
              (iter (cdr ps) nrps a b)
              (iter (cdr ps) nrps maxa maxb)))))
  (iter primes rprimes 0 0))
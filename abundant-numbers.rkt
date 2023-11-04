#lang racket
(require math/number-theory)

(define (sum li)
  (apply + li))

(define (fischer-test n)
  (cond
    [(> n 630) (= 0 (modulo n 12))]
    [(> n 20) (= 0 (modulo n 6))]
    [(> n 3) (= 0 (modulo n 2))]
    [else #t]))

(define (fischer-iterator n)
  (cond
    [(>= n 636) 12]
    [(>= n 24) 6]
    [(>= n 4) 2]
    [else 1]))

(define (highly-abundant-nums limit)
  (define (loop results n s)
    (cond 
      [(> n limit) (reverse results)]
      [(fischer-test n)
       (let [(sigma (sum (divisors n)))]
         (if (> sigma s)
             (loop (cons n results) (+ n 1) sigma)
             (loop results (+ n 1) s)))]
      [else (loop results (+ n 1) s)]))
  (loop (list) 1 0))

(define (highly-abundant-nums2 limit)
  (define (loop results n s)
    (cond 
      [(> n limit) (reverse results)]
      [else
       (let [(sigma (sum (divisors n)))
             (delta (fischer-iterator n))]
         (if (> sigma s)
             (loop (cons n results) (+ n delta) sigma)
             (loop results (+ n delta) s)))]))
      
  (loop (list) 1 0))
  

(define (modulo-test-loop limit n)
  (cond
    [(> n limit) #t]
    [else (let [(f (modulo n 12))]
            (modulo-test-loop limit (+ n 1)))]))
#lang racket
(require math/number-theory)

(define (compatible-numbers? a b)
  (equal? (prime-divisors a) (prime-divisors b)))

(define (reducible? g x)
  (let ([d (gcd g x)])
    (cond
      [(= d 1) #f]
      [(= d x) #t]
      [else (reducible? g (/ x d))])))

(define (compatible? a b)
  (cond
    [(= a b) #t]
    [(or (= a 1) (= b 1)) #f]
    [else
     (let [(g (gcd a b))]
       (if (= g 1) #f
           (and (reducible? g a)
                (reducible? g b))))]))
       
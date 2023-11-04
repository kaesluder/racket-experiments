#lang racket
;Find a 10-digit number, with all digits unique, such that the 
;first n digits of the number are divisible by n. For instance, 
;in the 3-digit number 345, the 1-digit prefix, 3, is divisible by 1, 
;the 2-digit prefix, 34, is divisible by 2, and the 3-digit prefix, 
;345, is divisible by 3.

(define (unique-digits? n)
  (define (loop n digits)
    ;;exit condition 1 n = 0
    (if (= n 0)
        #t
        (let [(x (modulo n 10))]
          ;;exit-condition 2 we've seen this digit
          (if (set-member? digits x)
              #f
              (loop (quotient n 10) (set-add digits x))))))
  (loop n (set)))


(define (gen-candidates current n)
  "Generate a list of candidates from the current value"
  ;;find values using (current % n) 
  ;;and iterating by n.
  (define next (* current 10))
  (define start 
    (cond
      ;;catch leading zeros
      [(= n 1)
       1]
      [(= 0 (modulo next n))
       next]
      [else
        (+ next (- n (modulo next n)))]))
  ;;generate our list from start to (current * 10) + 10
  ;;step by n
  ;;filter for unique digits. 
  (filter unique-digits? (range start (+ 10 (* 10 current)) n)))

(define (solutions max-length)
  (define results (list))
  (define (loop curr n)
    (if (> n max-length)
        (set! results (cons curr results))
        (for [(x (gen-candidates curr n))]
          (loop x (+ n 1)) results)))
  (loop 0 1)
  (reverse results))
       
(time (solutions 10))



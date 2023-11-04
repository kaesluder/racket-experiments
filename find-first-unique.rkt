#lang racket

(define (create-frequency-table li)
  (for/fold ([results (hash)])
    ([i li])
    (hash-set results i (add1 (hash-ref results i 0))))) 


;;find the first value in li for which
;;(proc (count i)) is #t
(define (find-first-frequency-f proc li)
  (let ([frequency-table (create-frequency-table li)])
    (for/or ([i li])
      (if (proc (hash-ref frequency-table i))
          i
          #f))))

(define (find-first-not-repeated li)
  (find-first-frequency-f (lambda (x) (= x 1)) li))

(define (find-first-even-freq li)
  (find-first-frequency-f even? li))
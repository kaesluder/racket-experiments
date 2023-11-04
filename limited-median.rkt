#lang racket

(define (limited-median xs)
  (define xsv (make-vector 256 0))
  (for ([x xs])
    (vector-set! xsv x (+ 1 (vector-ref xsv x))))
  (define half-limit (/ (length xs) 2))
  (define (median low high low-count high-count)
    (cond
      [(< low-count half-limit) 
       (median (add1 low) high (+ low-count (vector-ref xsv low)) high-count)]
      [(< high-count half-limit)
       (median low (sub1 high) low-count (+ high-count (vector-ref xsv high)))]
      [else
       (/ (+ low high) 2)]))
  (median 0 255 0 0))
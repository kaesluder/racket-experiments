#lang racket

(define (gen-random-list n)
  (for/list ([n (range n)])
    (random 1000)))

(define (binary-search x li)
  (define (loop min max)
    (if (< max min)
        #f
        (let ([curr (quotient (+ min max) 2)])
          (let ([val (list-ref li curr)])
            (cond [(= val x) val]
                  [(< val x) (loop (+ curr 1) max)]
                  [else (loop min (- curr 1))])))))
  (loop 0 (- (length li) 1)))
          
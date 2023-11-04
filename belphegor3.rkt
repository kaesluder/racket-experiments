#lang racket
(require math/number-theory)

(define (belphegor x)
  (+ (expt 10 (+ x x 4))
     (* 666 (expt 10 (+ x 1)))
     1))
(define (list-belphegor2 lim)
  (for/list ([i (range 0 (+ lim 1))]
             #:when (prime? (belphegor i)))
    i))
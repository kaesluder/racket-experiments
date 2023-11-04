#lang racket

(define (zeroes vec)
  (define (loop lo hi counter)
    (cond ((< hi lo) (values counter vec))
          ((zero? (vector-ref vec lo))
           (vector-set! vec lo (vector-ref vec hi))
           (vector-set! vec hi 0)
           (loop lo (- hi 1) counter))
          (else (loop (+ lo 1) hi (+ counter 1)))))
  (loop 0 (- (vector-length vec) 1) 0))


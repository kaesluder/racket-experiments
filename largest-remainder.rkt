#lang racket

(define (largest-remainder n d (lr 0) (lrd 0)) ;;lr = largest remainder so far lrd = divisor with largest remainder
  (cond
    [(= d 0) (values lr lrd)] ;;catch cases like n = 6, d = 3
    [(> lr d) (values lr lrd)] ;;stop when d is < largest remainder
    [(> (modulo n d) lr)
     (largest-remainder n (sub1 d) (modulo n d) d)] ;;save new largest remainder
    [else 
     (largest-remainder n (sub1 d) lr lrd)])) ;;iterate with d-1

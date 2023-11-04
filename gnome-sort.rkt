#lang racket

(define (gnome-sort vec (start 0))
  (let ([prev (- start 1)])
    (cond 
      [(= start 0) (gnome-sort vec 1)] ;starting condition: move forward one step
      [(= start (vector-length vec)) vec] ;end condition: spit out result
      [(>= (vector-ref vec start) (vector-ref vec prev)) 
       (gnome-sort vec (+ start 1))] ;pair already sorted: move forward
      [else ;pair not sorted: swap and move back
       (let ([prev-val (vector-ref vec prev)])
         (vector-set! vec prev (vector-ref vec start))
         (vector-set! vec start prev-val)
         (gnome-sort vec (- start 1)))])))
      
      
#lang racket
(require racket/set)

(define (list-member-assume-ordered? li v)
  (cond
    [(empty? li) #f]
    [(= v (car li)) #t]
    [(> v (car li)) #f]
    [else (list-member-assume-ordered? (cdr li) v)]))

(define (next-non-member li start)
  (cond
    [(set-empty? li) start]
    [(not (set-member? li start)) start]
    [else 
     (next-non-member li (add1 start))]))



(define (figure-figure n (i 1) (f (set)) (s (list)) (prev-f 0))
  (cond
    [(> i n) (values (sort (set->list f) <) (reverse s))]
    [(= i 1) (figure-figure n 2 (set 1) (list 2) 1)]
    [else
     (let* 
         ([next-f (+ prev-f (car s))]
          [f (set-add f next-f)]
          [next-s (next-non-member f (add1 (car s)))]
          [s (cons next-s s)])
       (figure-figure n (add1 i) f s next-f))]))
                    
  
  
  



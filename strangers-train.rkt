#lang racket/base
(require math/number-theory)
(require racket/set)

;;http://programmingpraxis.com/2015/10/27/strangers-on-a-train/


(define (coprime-list x n li) 
  ;;check if x is coprime to the n top elements of li
  (cond
    [(= n 0) #t] 
    [(not (coprime? x (car li))) #f]
    [else (coprime-list x (- n 1) (cdr li))]))

(define (strangers-find-next li)
  ;;find the next item in the strangers-in-a-train sequence
  (define prev-set (list->set li)) ;;use a set for speed
  (define n (quotient (+ 1 (length li)) 2)) 
  (define (loop x)
    (cond
      [(set-member? prev-set x)
       (loop (+ x 1))]
      [(coprime-list x n li)
       x] ;;exit
      [else
       (loop (+ x 1))]))
  (loop 3))

(define (strangers n (li (list 2)))
  ;;recursive with seeded list
  (cond 
    [(= n 1) (reverse li)] ;;exit
    [else 
     (strangers (- n 1) (cons (strangers-find-next li) li))]))







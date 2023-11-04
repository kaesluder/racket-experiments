#lang racket

(define (search-pop sym li (prev (list)))
  (cond 
   [(null? li) #f]
   [(equal? (car li) sym) (cons sym (append (reverse prev) (cdr li)))]
   [else
    (search-pop sym (cdr li) (cons (car li) prev))]))

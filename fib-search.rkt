#lang racket

(define (fibs k)
  (define (loop x y results n)
    (if (= 0 n)
        (reverse results)
        (loop (+ x y) x (cons x results) (- n 1))))
  (loop 1 0 (list) k))

(define (fibs-next-greater-than n)
    (define (loop x y results)
      (if (> x n)
          (cons x results)
          (loop (+ x y) x (cons x results))))
  (loop 1 0 (list)))

(define (gen-random-list n)
  (for/list ([n (range n)])
    (random 1000)))

(define (fibonacci-search x li)
  (define li-sorted (sort li <))
  (define fibs (fibs-next-greater-than (length li-sorted)))
  (define (loop li fs)
    (let ([current (list-ref li (- (car (cdr fs)) 1))])
      (cond [(null? fs) #f]
            [(null? li) #f]
            [(= x current) current]
            [(< x current) (loop (take li (car (cdr fs))) (cdr fs))]
            [else (loop (drop li (car (cdr fs))) (drop fs 2))])))
  (loop li-sorted fibs))
          
    
  
  
  
  
    


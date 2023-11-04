#lang racket


(define (parts2 stars bars)
  ;;if bars = 1, return a list with stars.
  (cond 
    [(= bars 1)
     (list (list stars))]
    ;;if stars = 0 make a list of 0s
    [(= stars 0)
     (list (make-list bars 0))]
    [else
     (for*/list
         ([h (range (+ 1 stars))]
          [rest (parts2 (- stars h) (- bars 1))])
       (cons h rest))]
         ;;create a list from 0 to stars
         ;;for each item in that list concatenate 
         ;;with (parts2 stars-i bars-1)
    ))


(define phone (list 16 4 17 10 15 4 4 6 7 14 9 17 27 6 1 9 0 12 20 8 0 3 4 0 3 4))

(define (diff-group li n target)
  (abs (- target (apply + (take li n)))))
  

(define (score partitions li target)
  (define (loop sum ps ls)
    (cond
      [(null? ps) sum]
      [else
       (let ((step (car ps)))
         ;(writeln ls)
         (loop (+ sum (diff-group ls step target))
               (cdr ps)
               (drop ls step)))]))
  (loop 0 partitions li))

(define (volume-partitions volume-number li)
  ;;define our target
  (define target (/ (apply + li) volume-number))
  (define candidates (parts2 26 volume-number))
  (define (score-curry candidate)
    (score candidate li target))
  (argmin score-curry candidates))
  
  
  



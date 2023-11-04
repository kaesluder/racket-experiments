#lang racket

(define (rotate li)
  (append (cdr li) (list (car li))))


(define gallons #(15 8 2 6 18 9 21 30))

(define miles #(8 6 30 9 15 21 2 18))

(define deltas (vector-map - gallons miles))

;;trivial case with no solution
(define deltas-error #(-1 -1 -1 -1 -1 -1 -1))

(define (find-start deltas)
  (define len (vector-length deltas))
  (define (loop start i sum)
    (cond
      ;;our return condition
      ;;if we've looped around to start,
      ;;return start
      [(= start (modulo i len))
       start]
      ;;error condition, there is no solution.
      ;;return #f
      [(> i (* 2 len))
       #f]
      ;;reset condition
      ;;if sum < 0 
      ;;start = i. i will loop around to 0.
      [(< sum 0)
       (loop (modulo i len) (+ 1 i) (vector-ref deltas (modulo i len)))]
      [else
       (loop start (+ 1 i) (+ sum (vector-ref deltas (modulo i len))))]))
  ;;start our loop with some
  ;;data to avoid early
  ;;termination. 
  (loop 0 1 (vector-ref deltas 0)))

(find-start deltas)







#lang racket
(require math/number-theory)

(define (factors-to-list n)
  (define factor-list (factorize n))
  (define (iter fl results)
    (if (null? fl)
        (reverse results)
        (letrec ([p (car fl)]
                 [x (first p)]
                 [y (second p)])
          (if (> y 1)
              (iter (cdr fl) (cons y (cons x results)))
              (iter (cdr fl) (cons x results))))))
  (iter factor-list (list)))


(define (list-to-integer li (result 0))
  (if (null? li)
      result
      (list-to-integer (cdr li) (+ (car li) (* (expt 10 (digits (car li))) result)))))

(define (digits n)
  (inexact->exact (floor (+ 1 (/ (log n) (log 10))))))

(define (next-conway n)
  (list-to-integer (factors-to-list n)))

(define (conway-chain n (results (list)))
  (let ([next (next-conway n)])
    ;;; (displayln n)
    (cond
      [(= next n) (reverse (cons n results))]
      [(prime? n) (reverse (cons n results))]
      [(> n (expt 10 20)) (printf "limit exceeded at ~a" (reverse (cons n results)))]
      [else (conway-chain next (cons n results))])))
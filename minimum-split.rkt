#lang racket

(define (partial-sums li (results (list)) (total 0))
  ;;note returns reverse list of partial sums
  (if (null? li)
      results
      (let* ([curr (car li)]
             [new-total (+ total curr)])
        (partial-sums (cdr li) (cons new-total results) new-total))))

(define (minimum-split li)
  ;;zero based index. 
  (let* ([sums (partial-sums li)]
         [total (car sums)])
    (let loop ([idx 1]
               [min-idx 0]
               [min total]
               [li sums])
      (cond
       [(null? li) (- (length sums) min-idx)]
       [(= total 0) -1]
       [(< (abs (- (car li) (- total (car li)))) min)
	(loop (add1 idx) idx (car li) (cdr li))]
       [else 
	(loop (add1 idx) min-idx min (cdr li))]))))


  
  
  

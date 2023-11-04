#lang racket/base

(require racket/set)

(define (number-to-list n [results (list)])
  (if (= n 0)
      results
      (number-to-list (quotient n 10) (cons (modulo n 10) results))))

(define (is-palendrome-number? n)
  (define li (number-to-list n))
  (equal? li (reverse li)))

(define (list-to-number li [result 0])
  (if (null? li)
      result
      (list-to-number (cdr li) (+ (car li) (* 10 result)))))

;;generate next number in lychrel chain. 
(define (next-reverse-add n)
  (+ n (list-to-number (reverse (number-to-list n)))))

;;;follow a chain to a palendrome, return list
;;;if chain ends in palendrome, otherwise #f.
(define (lychrel-chain n [b 100] [results (list)])
  (let ([nresults (cons n results)])
    (cond [(= b 0) #f] ;;probable lycrel
          [(is-palendrome-number? n) (reverse nresults)]
          [else
           (lychrel-chain (next-reverse-add n) (- b 1) nresults)])))


;;;helper function, return #t if probable lychrel.
(define (lychrel? n)
  (not (lychrel-chain n)))


;;;list all probable lycrels less than n;
(define (lychrels-less-than [n 1000] [results (list)]) 
  (cond [(= n 9) results]  ;;exit when n = 9 (single-digit numbers are palendromic)
        [(lychrel? n) ;;contine 1: probable lycrel (no chain) 
         (lychrels-less-than (- n 1) (cons n results))]
        [else ;;default continue count down
         (lychrels-less-than (- n 1) results)]))

  
;;;follow a chain to a palendrome, return list
;;;if chain ends in palendrome, otherwise #f.
(define (lychrel-chain-probable n [b 100] [results (list)])
  (let ([nresults (cons n results)])
    (cond [(= b 0) (reverse nresults)] ;;probable lycrel
          [(is-palendrome-number? n) #f]
          [else
           (lychrel-chain-probable (next-reverse-add n) (- b 1) nresults)])))


(define (list-in-set li st)
  (cond [(null? li) #f]
        [(set-member? st (car li)) #t]
        [else (list-in-set (cdr li) st)]))

(define (lychrel-seeds limit)
  (define (iter n seeds threads)
    (cond [(> n limit) seeds]
          [else
           (define new-thread (lychrel-chain-probable (next-reverse-add n)))
           (cond [(not new-thread) (iter (+ n 1) seeds threads)]
                 [(list-in-set new-thread threads) ;;kin number
                  (iter (+ n 1) seeds threads)]
                 [new-thread ;;new-thread exists but does not match
                  (iter (+ n 1) (cons n seeds) (set-union threads (list->set new-thread)))]
                 [else ;;not a seed or kin number
                  (iter (+ n 1) seeds threads)])]))
  (reverse (iter 1 (list) (set))))
                  
                 
  

(time (lychrels-less-than 1000))



  




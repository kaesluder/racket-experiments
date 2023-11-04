#lang racket
(require racket/list)

(define markov-depth (make-parameter 2))


(define filename (make-parameter "/home/kirk/Dropbox/pride_and_prejudice.txt"))

(command-line
 #:once-each
 [("-d") d "markov chain depth"
         (markov-depth (string->number d))]
 #:args f
 (when (not (empty? f))
   (filename (string->path (car f)))))


(define pride (file->string (filename)))
(define pride-list (regexp-split #px"\\s+" pride))

(define (markov-chains-old n li (results (list)))
  (if (< (length li) n)
      results
      (markov-chains n (cdr li) (cons (take li n) results))))

(define (markov-chains n li)
  (define livec (list->vector li))
  (for/list ([i (range (- (length li) n))])
    (for/list ([j (range n)])
      (vector-ref livec (+ i j)))))

    

(define (markov-table chain-list)
  (define results (make-hash))
  (for ([item chain-list])
    (let ([key (drop-right item 1)])
      (let ([old (hash-ref results key (list))])
        (hash-set! results key (cons (last item) old)))))
  results)

(define (markov-next table prev)
  (let ([next-list (hash-ref table prev)])
    (list-ref next-list (random (length next-list)))))


(define (markov-text-list table n seed)
  (define (iter i seed results)
    (if (= i n)
        results
        (let ([next (markov-next table seed)])
          (iter
           (+ i 1)
           (append (drop seed 1) (list next))
           (cons next results)))))
  (reverse (iter 0 seed (list))))

(define (markov-random-text table n)
  (let ([keys (hash-keys table)])
    (markov-text-list table n (list-ref keys (random (length keys))))))

(define (print-list li)
  (for ([s li])
    (printf "~a " s)))

(define pride-table (markov-table (markov-chains (markov-depth) pride-list)))

(print-list (markov-random-text pride-table 500))

(printf "\n")
    
    


    

  
  
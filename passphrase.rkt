#lang racket/base
(require racket/file)
(require racket/set)
(require racket/list)

(define sourcefile 
  (if (< 0 (vector-length (current-command-line-arguments)))
      (vector-ref (current-command-line-arguments) 0)
      "../monte_cristo.txt"))
(displayln sourcefile)
(define lines (file->lines sourcefile))

(define word-re (pregexp  "\\w+"))

(define (string->words line)
  (define (not-alpha? c)
    (not (char-alphabetic? c)))
  (define (loop words characters)
    (if (null? characters)
        words
        (if (char-alphabetic? (car characters))
            (loop 
             (cons (list->string (takef characters char-alphabetic?)) words)
             (dropf characters char-alphabetic?))
            (loop
             words
             (dropf characters not-alpha?)))))
  (loop '() (string->list line)))
  

                                    
(define (gen-wordlist-old)

  (for*/fold ([wordlist (set)])
    ([line lines]
     [word (regexp-match* word-re line)])
    (set-add wordlist (string-downcase word))))

(define (gen-wordlist)
  (define results (mutable-set))
  (for*
      ([line lines]
       [word (regexp-match* word-re line)])
    (let ((lcword (string-downcase word)))
      (when (not (set-member? results lcword))
        (set-add! results lcword))))
  results)


(define (gen-wordlist2)
  (define results (make-hash))
  (for*
      ([line lines]
       [word (regexp-match* word-re line)])
    ;;(hash-set! results (string-downcase word) #t))
    
    (let ((lcword (string-downcase word)))
      (when (not (hash-ref results lcword #f))
        (hash-set! results lcword #t))))
  (hash-keys results))



;;(time (set-count (gen-wordlist)))
(define wordlist (gen-wordlist))

(fprintf (current-output-port) "~a unique words~n" (set-count wordlist))


(define (display-passphrase)
  (let ([words-as-list (set->list wordlist)])
    (displayln
     (for/list ([i (range 7)])
       (list-ref words-as-list (random (set-count wordlist)))))))

(display-passphrase)









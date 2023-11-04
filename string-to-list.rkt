#lang racket

(define (string->words str)
  (define (loop wordlist cur characters)
    (if (null? characters)
        (if (not (null? wordlist))
            (cons (list->string cur) wordlist)
            wordlist)
        (if (char-alphabetic? (car characters))
            (loop wordlist (cons (car characters) cur) (cdr characters))
            (loop 
             ;;test to see if cur is empty
             ;;if not push it onto the list
             (if (not (null? cur))
                 (cons (list->string cur) wordlist)
                 wordlist)
             '()
             (cdr characters)))))
  (loop '() '() (reverse (string->list str))))
            
        


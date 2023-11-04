#lang racket/base
(require racket/list)
(require racket/file)

(define (ordered-word? word)
  ;;one-character words are ordered
  ;;catch them to avoid an error with
  ;;apply.
  (if (= (string-length word) 1)
      #t
      ;;use case-insensitive character comparison
      (apply char-ci<=? (string->list word))))

(define (words) (file->lines "/usr/share/dict/words"))

(define (ordered-words)
  (filter ordered-word? (words)))


(define (longest li)
  (argmax string-length li))

(define (words-longer-than li n)
  (filter (lambda (word) (>= (string-length word) n)) li))

(time (words-longer-than (ordered-words) 7))
      


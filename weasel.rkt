#lang racket/base
(require racket/list)

;;Dawkins's Weasel.

(define letters "abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(define (random-string len letter-list)
  "Create a random string of len letters from a list of letters."
  (define char-num (string-length letter-list))
  (apply string ;;convert a list of characters into a string.
         (for/list [(i (range len))]
           (string-ref letter-list (random char-num)))))

(define (random-char)
  "Pull a single random character."
  (string-ref letters (random (string-length letters))))

(define (mutate-this?)
  "Just a function to return true 5% of the time."
  (< (random) 0.05))

(define (mutate-string str)
  "Iterate over the string and mutate characters."
  (apply string
         (for/list [(char str)]
           (if (mutate-this?)
               (random-char)
               char))))

(define (reproduce parent n)
  "Create a list of n mutants from parent."
  (for/list [(i (range n))]
    (mutate-string parent)))

(define weasel "Methinks it is like a weasel")

(define (score target candidate)
  "Return a numeric score comparing two strings character by character."
  (for/sum [(a target)
            (b candidate)]
    (if (char=? a b)
        1
        0)))

(define (evolve target)
  "Evolve by randomly mutating children of target until a string identical to target is produced"
  
   
  (define (score-curry child)
    "Convenience function."
    (score target child))
  (define (evolve-loop ancestor count)
    (displayln ancestor)
    (if (string=? ancestor target) ;;exit condition
        count ;;return count when match is found
        (evolve-loop
         (argmax score-curry ;;score all the children 
                 ;;use the one with the max score as the next ancestor
            (reproduce ancestor 100))
         (+ 1 count))))
  (evolve-loop 
    (random-string (string-length target) letters) 0))


(define (evolve-converge n target)
  
   
  (define (score-curry child)
    "Convenience function."
    (score target child))
  (define (converge-loop i parent results)
    (if (or (string=? target parent)
            (= i n))
        results
        (let ((new-ancestor (argmax score-curry ;;score all the children 
                 ;;use the one with the max score as the next ancestor
            (reproduce parent 100))))
          (converge-loop (+ 1 i)
                         new-ancestor
                         (cons (score-curry new-ancestor) results)))))
  (converge-loop 0 (random-string (string-length target) letters) (list)))
        
;;(take (evolve-converge 5000 (random-string 500 letters)) 20)



(evolve weasel)
    
        





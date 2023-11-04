#lang racket

(define (belphegor x)
  (+ (expt 10 (+ x x 4))
     (* 666 (expt 10 (+ x 1)))
     1))


(define (isqrt n)
  (if (not (and (positive? n) (integer? n)))
      (error 'isqrt "must be positive integer")
      (let loop ((x n))
        (let ((y (quotient (+ x (quotient n x)) 2)))
          (if (< y x) (loop y) x)))))

(define (square? n)
  (let ((n2 (isqrt n)))
    (= (* n2 n2) n)))

(define (expm b e m)
  (define (m* x y) (modulo (* x y) m))
  (cond ((zero? e) 1)
        ((even? e) (expm (m* b b) (/ e 2) m))
        (else (m* b (expm (m* b b) (/ (- e 1) 2) m)))))

(define (jacobi a m)
  (if (not (integer? a)) (error 'jacobi "must be integer")
    (if (not (and (integer? m) (positive? m) (odd? m)))
        (error 'jacobi "modulus must be odd positive integer")
        (let loop1 ((a (modulo a m)) (m m) (t 1))
          (if (zero? a) (if (= m 1) t 0)
            (let ((z (if (member (modulo m 8) (list 3 5)) -1 1)))
              (let loop2 ((a a) (t t))
                (if (even? a) (loop2 (/ a 2) (* t z))
                  (loop1 (modulo m a) a
                         (if (and (= (modulo a 4) 3)
                                  (= (modulo m 4) 3))
                             (- t) t))))))))))

(define (strong-pseudoprime? n a)
  (let loop ((r 0) (s (- n 1)))
    (if (even? s) (loop (+ r 1) (/ s 2))
      (if (= (expm a s n) 1) #t
        (let loop ((r r) (s s))
          (cond ((zero? r) #f)
                ((= (expm a s n) (- n 1)) #t)
                (else (loop (- r 1) (* s 2)))))))))

(define (selfridge n)
  (let loop ((d-abs 5) (sign 1))
    (let ((d (* d-abs sign)))
      (cond ((< 1 (gcd d n)) (values d 0 0))
            ((= (jacobi d n) -1) (values d 1 (/ (- 1 d) 4)))
            (else (loop (+ d-abs 2) (- sign)))))))

(define (lucas p q m n) ; right-to-left
  (define (even e o) (if (even? n) e o))
  (define (mod n) (if (zero? m) n (modulo n m)))
  (let ((d (- (* p p) (* 4 q))))
    (let loop ((un 1) (vn p) (qn q) (n (quotient n 2))
               (u (even 0 1)) (v (even 2 p)) (k (even 1 q)))
      ; (display un) (display " ") (display vn) (display " ")
      ; (display qn) (display " ") (display n) (display " ")
      ; (display u) (display " ") (display v) (display " ")
      ; (display k) (newline)
      (if (zero? n) (values u v k)
        (let ((u2 (mod (* un vn))) (v2 (mod (- (* vn vn) (* 2 qn))))
              (q2 (mod (* qn qn))) (n2 (quotient n 2)))
          (if (even? n) (loop u2 v2 q2 n2 u v k)
            (let* ((uu (+ (* u v2) (* u2 v)))
                   (vv (+ (* v v2) (* d u u2)))
                   (uu (if (and (positive? m) (odd? uu)) (+ uu m) uu))
                   (vv (if (and (positive? m) (odd? vv)) (+ vv m) vv))
                   (uu (mod (/ uu 2))) (vv (mod (/ vv 2))))
              (loop u2 v2 q2 n2 uu vv (* k q2)))))))))

(define (powers-of-two n)
  (let loop ((s 0) (n n))
    (if (odd? n) (values s n)
      (loop (+ s 1) (/ n 2)))))

(define (strong-lucas-pseudoprime? n)
  ; assumes odd positive integer not a square
  (call-with-values
    (lambda () (selfridge n))
    (lambda (d p q)
      (if (zero? p) (= n d)
        (call-with-values
          (lambda () (powers-of-two (+ n 1)))
          (lambda (s t)
            (call-with-values
              (lambda () (lucas p q n t))
              (lambda (u v k)
                (if (or (zero? u) (zero? v)) #t
                  (let loop ((r 1) (v v) (k k))
                    (if (= r s) #f
                      (let* ((v (modulo (- (* v v) (* 2 k)) n))
                             (k (modulo (* k k) n)))
                        (if (zero? v) #t (loop (+ r 1) v k))))))))))))))

(define prime?
  (let ((ps '(2 3 5 7 11 13 17 19 23 29 31 37 41
              43 47 53 59 61 67 71 73 79 83 89 97)))
    (lambda (n)
      (if (not (integer? n)) (error 'prime? "must be integer") #t)
      (if (or (< n 2) (square? n)) #f
        (let loop ((ps ps))
          (if (pair? ps)
              (if (zero? (modulo n (car ps))) (= n (car ps)) (loop (cdr ps)))
              (and (strong-pseudoprime? n 2)
                   (strong-pseudoprime? n 3)
                   (strong-lucas-pseudoprime? n))))))))

(define (list-belphegor lim)
  (define (iter n results)
    (if (> n lim)
        results
        (if (prime? (belphegor n))
            (iter (+ n 1) (cons n results))
            (iter (+ n 1) results))))
  (reverse (iter 0 (list))))

(define (list-belphegor2 lim)
  (for/list ([i (range 0 (+ lim 1))]
             #:when (prime? (belphegor i)))
    i))

;(list-belphegor2 100)



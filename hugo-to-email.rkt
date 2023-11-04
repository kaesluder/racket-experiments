#lang racket
(require markdown)
(require markdown/display-xexpr)
(require net/smtp)
(require net/head)
(require openssl)

(define base-url "https://www.bigeekfan.com/")

;; load the file into a string
(define (load-file filename)
  (file->string filename))

(define test-filename "/Users/ksluder/hugo-blog/content/post/20170517_star_trek_tos_charliex.md")


;; clean up relative links
(define (clean-relative-links string base-url)
  (string-replace string "](/"
                  (string-join (list "](" base-url) "")))

;; pull the title using regex
(define (get-title string)
  (for/first ([line (string-split string "\n")]
              #:when (regexp-match? #rx"title:" line))
    (second (regexp-match #rx"\"(.*)\"" line))))


;; the the position of the start of the body of the post
(define (get-body-index string)
  (cdr (first (regexp-match-positions #rx"\n---\n\n" string))))

;; get the body
(define (get-body string)
  (substring string (get-body-index string)))

;; parse markdown and render
(define (render-body string)
  (xexpr->string `(div () ,@(parse-markdown (get-body string)))))

;;; command line argument parsing

(define filename (make-parameter #f))

(when (< 0 (vector-length (current-command-line-arguments)))
  (filename (last (vector->list (current-command-line-arguments)))))

(if (filename)
  (display (render-body (load-file (filename))))
  (display "No file."))

;;; email

(define smtp-pass "7tqy367kcmcdhxuu")
(define smtp-user "kirkjobsluder@fastmail.fm")
(define smtp-server "smtp.fastmail.com")
(define smtp-port 465)

(define test-header
  (standard-message-header smtp-user
                           (list smtp-user)
                           '()
                           '()
                           "Test message"))

(define (email-body html-string)
  (string-split html-string "\n"))

(define (test-send-message html-string)
  (smtp-send-message
   smtp-server
   smtp-user
   (list smtp-user)
   test-header
   (email-body html-string)
   #:port-no smtp-port
   #:auth-user smtp-user
   #:auth-passwd smtp-pass
   #:tcp-connect ssl-connect))




 
 


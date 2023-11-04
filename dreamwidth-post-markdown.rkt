#!/usr/bin/env racket
#lang racket
(require net/smtp)
(require net/head)
(require openssl)

(define smtp-pass (string-trim (with-output-to-string (lambda () (system "gpg -d ~/bin/secret.txt.gpg")))))
(define smtp-user "kirkjobsluder@fastmail.fm")
(define smtp-server "smtp.fastmail.com")
(define smtp-port 465)
(define base-url "https://www.bigeekfan.com/")
(define dreamwidth-address "cbrachyrhynchos+214335@post.dreamwidth.org")

(displayln smtp-pass)



;; load the file into a string
(define (load-file filename)
  (file->string filename))

(define test-filename "/Users/ksluder/hugo-blog/content/post/20170517_star_trek_tos_charliex.md")


(define test-header
  (standard-message-header smtp-user
                           (list smtp-user)
                           '()
                           '()
                           "Test message"))

(define (create-header recipient-list subject)
  (standard-message-header smtp-user
                           recipient-list
                           '()
                           '()
                           subject))

(define (email-body html-string)
  (string-split html-string "\n"))


;;; send an email message to recipient-list with the subject
;;; body.
(define (send-message recipient-list subject body-string)
  (smtp-send-message
   smtp-server
   smtp-user
   recipient-list
   (create-header recipient-list subject)
   (create-email-body body-string)
   #:port-no smtp-port
   #:auth-user smtp-user
   #:auth-passwd smtp-pass
   #:tcp-connect ssl-connect))


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

;;; combine all of the above functions to create an email body
;;; as a list of strings including the !markdown directive for
;;; dreamwidth. 
(define (create-email-body string)
  (append (list "!markdown" "") (email-body (get-body (clean-relative-links string base-url)))))

;;; send a hugo markdown file to recipient list
(define (send-markdown-file recipient-list filename)
  (define text (load-file filename))
  (define subject (get-title text))
  (send-message recipient-list subject text))
  

;;; command line argument parsing

(define filename (make-parameter #f))

(when (< 0 (vector-length (current-command-line-arguments)))
  (filename (last (vector->list (current-command-line-arguments)))))





(if (filename)
    (send-markdown-file (list "kirkjobsluder@fastmail.fm" dreamwidth-address) (filename))
    (display "No file."))


 
 
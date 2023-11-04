#lang racket
(require html-parsing)
(require sxml)
(require net/url)
(require net/url-string)



(define testfile (open-input-file "index.html"))

(define testxexp (html->xexp testfile))

(define (anchor-nodes xexp)
  ;;; return a list of anchor nodes
  ((sxpath '(// a)) xexp))

(define (extract-url node)
  ;;; extract a url from node
  (let ((attrs (sxml:attr-list node)))
    (for/first ([attr attrs]
                #:when (eq? (car attr) 'href))
      (second attr))))

(define (get-http-response url-string)
  (first (port->lines (head-impure-port (string->url url-string)))))

(with-handlers ([exn:fail? (lambda (exn)
                               (displayln (exn-message exn))
                               (exn-message exn))])
    (head-impure-port (string->url "http://localhost/")))

      



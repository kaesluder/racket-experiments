#lang racket
(require net/http-client)
(require net/url)
(require xml)

(define (fetch-pixel)
  (string->xexpr
   (port->string
    (get-pure-port
     (string->url "http://pixelcurious.tumblr.com/rss")))))
                  
  
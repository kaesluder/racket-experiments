#lang racket
(require html-parsing)
(require sxml)

(define foo (html->xexp (open-input-file "/home/kirk/Dropbox/hugo-blog-test/public/index.html")))
(define bar ((sxml:modify (list "//link" 'delete)) foo))
(display (srl:sxml->html foo))
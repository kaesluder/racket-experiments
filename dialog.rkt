#lang racket
(require racket/gui/base)

(define frame (new frame% [label "Power Management"]))
(define msg (new message% [parent frame]
                          [label "No events so far..."]))
(new button% [parent frame]
             [label "Click Me"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send msg set-label "Button click"))])

(new button% [parent frame]
             [label "Click Me 2"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send msg set-label "Button click 2"))])
(send frame show #t)
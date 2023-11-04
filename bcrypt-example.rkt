#lang racket/base
(require bcrypt)
(require racket/list)
(require sha)
(require net/base64)
(encode (string->bytes/utf-8 "foo") #:rounds 9)
;;note convert strings to byte strings. 
(check #"$2y$09$YUAjXRkccq/Q.hiLBxsKLO4TylaH1ZJ.Y.b6qYS8v.ceW9L91Y0lm\0" #"foo")

(for ([i (range 4 15)])
  (printf "Difficulty ~a: " i)
  (time (encode #"foo" #:rounds i)))

(base64-encode (sha256 #"foo"))


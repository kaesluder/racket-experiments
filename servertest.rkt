#lang racket
; specify the right language
; include the correct libraries
(require web-server/servlet
         ; this one gets "serve/servlet"
         web-server/servlet-env)
(require bcrypt)

(define stored-username "foo@foo.com")
(define stored-password #"$2y$09$vhpl6yGLBnc7kpYzQYgdvedcrJGBGIDmMadmvaneTG0wIOk1XINu.\0")

(define (start request)
  ;;(print request)
  (blog-dispatch request))

;;dispatch functions by url
(define-values (blog-dispatch blog-url)
  (dispatch-rules
   (("") list-posts)
   (("login") #:method (or "get" "post") login-page)
   (("bindings") binding-page)
   (else list-posts)))

(define (list-posts req)
  (response/output
   (lambda (op)
     (write-bytes #"<html><head><title>Hello</title><body><p>Hello</body></html>" op) (void))))


;;; check if bindings exist within a request
(define (exists-binding-list? req bindings-list)
  (let ((bind (request-bindings req)))
    (for/and ((sym bindings-list))
      (exists-binding? sym bind))))

;;; check login against stored values
(define (valid-login? req)
  ;test #1 check if username and password exist
  ;in the request
  (if (not (exists-binding-list? req (list 'username 'password)))
      #f
      ;set bindings
      (letrec ((bind (request-bindings req))
               (username (extract-binding/single 'username bind))
               (password (extract-binding/single 'password bind)))
        ;test #2 username equality
        (if (not (string=? username stored-username))
            #f
            ;test #3 password validity
            (check stored-password (string->bytes/utf-8 password))))))
        

;;; login page logic
(define (login-page req)
  (if (valid-login? req)
      ;redirect
      (redirect-to "/")
      (response/xexpr
       `(html (head (title "Login"))
              (body (form ([method "POST"])
                          (div 
                           (label "Username:")
                           (input ((type "email") (name "username"))))
                          (div
                           (label "Password:")
                           (input ((type "password") (name "password"))))
                          (div
                           (input ((type "submit"))))
                          (div)))))))



; starts a web server where...
(serve/servlet start ; answers requests
               #:servlet-path "" ; is the default URL
               #:port 8080 ; is the port
               #:servlet-regexp #rx"") ; is a regexp decide
                                       ; if 'start' should
                                       ; handle the request
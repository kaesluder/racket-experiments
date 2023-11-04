#lang web-server/insta

;(define (start request)
;  (response/xexpr
;   '(html
;     (head (title "My Blog"))
;     (body (h1 "Under Construction")))))

(struct post (title body comments)
  #:mutable)

(define (post-insert-comment! a-post a-comment)
  (set-post-comments! a-post
                      (append (post-comments a-post) (list a-comment))))
  

(struct blog (posts) #:mutable)

(define (blog-insert-post! a-blog a-post)
  (set-blog-posts! a-blog
                   (cons a-post (blog-posts a-blog))))

(define first-post (post "First Post"
                         "Hey, this is a first post."
                         (list "snapple" "glumph")))

(define (render-comment a-comment)
  `(li ((class "comment")) ,a-comment))

(define BLOG
  (blog
   (list first-post
                   (post "Foo"
                         "Bar!"
                         (list)))))

(define (render-post post)
  `(div ((class "post"))
        (h3 ((class "post-title")) ,(post-title post))
        (p ,(post-body post))
        (ul ((class "comments"))
            ,@(map render-comment (post-comments post)))))

(define (render-posts)
  `(div ((class "posts")) ,@(map render-post (blog-posts BLOG))))

; render-blog-page: blog request -> doesn't return
; Consumes a blog and a request, and produces an HTML page
; of the content of the blog.
(define (render-blog-page request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "My Blog"))
            (body
             (h1 "My Blog")
             ,(render-posts)
             (form ((action
                     ,(embed/url insert-post-handler))
                    (method "post"))
                   (input ((name "title")))
                   (input ((name "body")))
                   (input ((type "submit"))))))))
 
  (define (insert-post-handler request)
    (blog-insert-post!
     BLOG
     (parse-post (request-bindings request)))
    (render-blog-page request))
  (send/suspend/dispatch response-generator))

; start: request -> response
; Consumes a request and produces a page that displays all of the
; web content.
(define (start request)
  (define a-blog
    (cond [(can-parse-post? (request-bindings request))
           (cons (parse-post (request-bindings request))
                 BLOG)]
          [else
           BLOG]))
  (render-blog-page request))


(define (can-parse-post? bindings)
  (and (exists-binding? 'title bindings)
       (exists-binding? 'body bindings)))

(define (parse-post bindings)
  (post (extract-binding/single 'title bindings)
        (extract-binding/single 'body bindings)
        (list)))


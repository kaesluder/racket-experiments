#lang racket
(require html-parsing)
(require sxml)
(require racket/date)


;;(define dom (html->xexp (open-input-file "/home/kirk/Downloads/bookmarks_all_20170923_002955.html")))

(define (list-items dom)
  ((sxpath '(dl (*or* dt dd))) dom))

(define (org-print-tags tags-as-list)
  (format ":~a:" (string-join tags-as-list ":")))

(define (org-format-date date)
  (define days-of-week #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
  (format "~a-~a-~a ~a ~a:~a:~a"
          (date-year date)
          (date-month date)
          (date-day date)
          (vector-ref days-of-week (date-week-day date))
          (date-hour date)
          (date-minute date)
          (date-second date)))
          
  
  

(define (seconds-to-date-string seconds)
  (define d (seconds->date seconds))
  (org-format-date d))

(define (org-print-link element)
  (define link (first ((sxpath '(// a)) element)))
  (let ([href (sxml:attr link 'href)]
        [text (sxml:text link)]
        [tags (string-split (sxml:attr link 'tags) ",")]
        [timestamp (sxml:attr link 'add_date)])
    (printf "** [[~a][~a]] ~a\n" href text (org-print-tags tags))
    (printf ":PROPERTIES:\n:CREATED: [~a]\n:END:\n\n" (seconds-to-date-string (string->number timestamp)))))

(define (org-print-description element)
  (printf "~a\n\n" (sxml:text element)))

(define (org-print-dl element-list)
  (for ([element element-list])
    (if (equal? (sxml:name element) 'dt)
        (org-print-link element)
        (org-print-description element))))

;;; command line argument parsing

(define filename (make-parameter #f))

(when (< 0 (vector-length (current-command-line-arguments)))
  (filename (last (vector->list (current-command-line-arguments)))))

(when (filename)
    (let ([dom (html->xexp (open-input-file (normalize-path (filename))))])
      (org-print-dl (list-items dom))))
      





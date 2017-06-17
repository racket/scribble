#lang racket/base
(require scribble/doclang
         scribble/core
         (except-in scribble/base author title)
         scribble/acmart
         scribble/latex-prefix
         racket/list
         "../private/defaults.rkt"
         (for-syntax racket/base))
(provide (except-out (all-from-out scribble/doclang) #%module-begin)
         (all-from-out scribble/acmart)
         (all-from-out scribble/base)
         (rename-out [module-begin #%module-begin]))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ id . body)
     (let ([format? #f]
           [review? #f]
           [screen? #f]
           [natbib? #f]
           [anonymous? #f]
           [authorversion? #f]
           [font-size #f])
       (let loop ([stuff #'body])
         (syntax-case* stuff (manuscript acmsmall acmlarge acmtog sigconf siggraph sigplan sigchi sigchi-a
					 review screen natbib anonymous authorversion 9pt 10pt 11pt 12pt)
		       (lambda (a b) (eq? (syntax-e a) (syntax-e b)))
           [(ws . body)
            ;; Skip intraline whitespace to find options:
            (and (string? (syntax-e #'ws))
                 (regexp-match? #rx"^ *$" (syntax-e #'ws)))
            (loop #'body)]

	   ; boolean options
	   [((review #t) . body)
	    (set! review? "review=true")
	    (loop #'body)]
	   [((review #f) . body)
	    (set! review? "review=false")
	    (loop #'body)]
	   [(review . body)
	    (set! review? "review=true")
	    (loop #'body)]
	   [((screen #t) . body)
	    (set! screen? "screen=true")
	    (loop #'body)]
	   [((screen #f) . body)
	    (set! screen? "screen=false")
	    (loop #'body)]
	   [(screen . body)
	    (set! screen? "screen=true")
	    (loop #'body)]
	   [((natbib #t) . body)
	    (set! natbib? "natbib=true")
	    (loop #'body)]
	   [((natbib #f) . body)
	    (set! natbib? "natbib=false")
	    (loop #'body)]
	   [(natbib . body)
	    (set! natbib? "natbib=true")
	    (loop #'body)]

	   [((anonymous #t) . body)
	    (set! anonymous? "anonymous=true")
	    (loop #'body)]
	   [((anonymous #f) . body)
	    (set! anonymous? "anonymous=false")
	    (loop #'body)]
	   [(anonymous . body)
	    (set! anonymous? "anonymous=true")
	    (loop #'body)]
	   [((authorversion #t) . body)
	    (set! authorversion? "authorversion=true")
	    (loop #'body)]
	   [((authorversion #f) . body)
	    (set! authorversion? "authorversion=false")
	    (loop #'body)]
	   [(authorversion . body)
	    (set! authorversion? "authorversion=true")
	    (loop #'body)]
	   [(9pt . body)
	    (set! font-size "9pt")
	    (loop #'body)]
	   [(10pt . body)
	    (set! font-size "10pt")
	    (loop #'body)]
	   [(11pt . body)
	    (set! font-size "11pt")
	    (loop #'body)]
	   [(12pt . body)
	    (set! font-size "12pt")
	    (loop #'body)]
	   
	   
           ; format options
           [(manuscript . body)
            (set! format? "manuscript")
            (loop #'body)]
           [(acmsmall . body)
            (set! format? "acmsmall")
            (loop #'body)]
           [(acmlarge . body)
            (set! format? "acmlarge")
            (loop #'body)]
           [(acmtog . body)
            (set! format? "acmtog")
            (loop #'body)]
           [(sigconf . body)
            (set! format? "sigconf")
            (loop #'body)]
           [(sigconf . body)
            (set! format? "siggraph")
            (loop #'body)]
           [(sigplan . body)
            (set! format? "sigplan")
            (loop #'body)]
           [(sigchi . body)
            (set! format? "sigchi")
            (loop #'body)]
           [(sigchi-a . body)
            (set! format? "sigchi-a")
            (loop #'body)]
	    
	   [body
            #`(#%module-begin id (post-process #,review? #,screen? #,natbib? #,anonymous? #,authorversion? #,font-size #,format?) () . body)])))]))

(define ((post-process . opts) doc)  
  (let ([options
         (if (ormap values opts)
             (format "[~a]" (apply string-append (add-between (filter values opts) ", ")))
             "")])
    (add-acmart-styles 
     (add-defaults doc
                   (string->bytes/utf-8
                    (format "\\documentclass~a{acmart}\n~a"
                            options
                            unicode-encoding-packages))
                   (scribble-file "acmart/style.tex")
                   (list (scribble-file "acmart/acmart.cls"))
                   #f
		   #:replacements (hash "scribble-load-replace.tex" (scribble-file "acmart/acmart-load.tex"))))))

(define (add-acmart-styles doc)
  ;; Ensure that "acmart.tex" is used, since "style.tex"
  ;; re-defines commands.
  (struct-copy part doc [to-collect
                         (cons invisible-element-to-collect-for-acmart-extras
                               (part-to-collect doc))]))

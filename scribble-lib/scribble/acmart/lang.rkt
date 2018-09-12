#lang racket/base
(require scribble/doclang
         scribble/core
         (except-in scribble/base author title)
         scribble/acmart
         scribble/latex-prefix
         racket/list
         "../private/defaults.rkt"
         (for-syntax racket/base
                     syntax/parse))
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
           [font-size #f]
           [nonacm? #f]
           [timestamp? #f]
           [author-draft? #f]
           [acmthm? #f])
       (let loop ([stuff #'body])
         (syntax-parse stuff
           #:datum-literals (manuscript acmsmall acmlarge acmtog sigconf siggraph sigplan sigchi
                                        sigchi-a dtrap pacmcgit tiot tdsci review screen natbib
                                        anonymous authorversion 9pt 10pt 11pt 12pt nonacm timestamp
                                        authordraft acmthm)
           
           ;; Skip intraline whitespace to find options:
           [(ws . body)
            #:when (and (string? (syntax-e #'ws))
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
           [((nonacm #t) . body)
            (set! nonacm? "nonacm=true")
            (loop #'body)]
           [((nonacm #f) . body)
            (set! nonacm? "nonacm=false")
            (loop #'body)]
           [(nonacm . body)
            (set! nonacm? "nonacm=true")
            (loop #'body)]
           [(timestamp . body)
            (set! timestamp? "timestamp=true")
            (loop #'body)]
           [((timestamp #t) . body)
            (set! timestamp? "timestamp=true")
            (loop #'body)]
           [((timestamp #f) . body)
            (set! timestamp? "timestamp=false")
            (loop #'body)]
           [(authordraft . body)
            (set! author-draft? "authordraft=true")
            (loop #'body)]
           [((authordraft #t) . body)
            (set! author-draft? "authordraft=true")
            (loop #'body)]
           [((authordraft #f) . body)
            (set! author-draft? "authordraft=false")
            (loop #'body)]
           [(acmthm . body)
            (set! acmthm? "acmthm=true")
            (loop #'body)]
           [((acmthm #t) . body)
            (set! acmthm? "acmthm=true")
            (loop #'body)]
           [((acmthm #f) . body)
            (set! acmthm? "acmthm=false")
            (loop #'body)]
	   
           ; format options
           [((~and fmt
                   (~or manuscript
                        acmsmall
                        acmlarge
                        acmtog
                        sigconf
                        siggraph
                        sigplan
                        sigchi
                        sigchi-a
                        dtrap
                        pacmcgit
                        tiot
                        tdsci))
             . body)
            (set! format? (symbol->string (syntax->datum #'fmt)))
            (loop #'body)]
           
           [body
            #`(#%module-begin id (post-process #,review? #,screen? #,natbib? #,anonymous?
                                               #,authorversion? #,font-size #,nonacm? #,timestamp?
                                               #,author-draft? #,acmthm? #,format?) () . body)])))]))

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
  (struct-copy part doc
               [to-collect
                ;; Ensure that "acmart.tex" is used, since "style.tex"
                ;; re-defines commands.
                (cons invisible-element-to-collect-for-acmart-extras
                      (part-to-collect doc))]
               [style (let ([s (part-style doc)])
                        (struct-copy style s
                                     [properties
                                      ;; Immitate Latex-based links where only the
                                      ;; number part is hyperlinked.
                                      (cons (link-render-style 'number)
                                            (style-properties s))]))]))

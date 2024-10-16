#lang racket/base
(require (for-syntax racket/base
                     racket/list
                     racket/stxparam-exptime)
         file/gunzip
         net/ftp
         racket/file
         racket/stxparam
         scribble/core
         scribble/decode
         scribble/doclang
         scribble/html-properties
         scribble/latex-prefix
         scribble/latex-properties
         setup/collects
         (except-in scribble/base author)
         (prefix-in s/b: scribble/base)
         "../private/defaults.rkt")

(module test racket/base)

(provide (except-out (all-from-out scribble/doclang) #%module-begin)
         (all-from-out scribble/base)
         (rename-out [module-begin #%module-begin])
         frontmatter
         abstract author email address)

(define-syntax (module-begin stx)
  ;; No options, currently, but keep in case we want to support some:
  (syntax-case* stx () (lambda (a b) (eq? (syntax-e a) (syntax-e b)))
    [(_ id ws . body)
     ;; Skip intraline whitespace to find options:
     (and (string? (syntax-e #'ws))
          (regexp-match? #rx"^ *$" (syntax-e #'ws)))
     #'(module-begin id . body)]
    [(_ id . body)
     #'(#%module-begin id (post-process) () . body)]))

(define ((post-process) doc)
  (add-defaults doc
                (string->bytes/utf-8 (string-append "\\documentclass{elsarticle}\n"
                                                    unicode-encoding-packages))
                (scribble-file "elsarticle/style.tex")
                '()
                #f))

(define elsarticle-extras
  (let ([abs (lambda (s)
               (path->collects-relative
                (collection-file-path s "scribble" "elsarticle")))])
    (list
     (make-tex-addition (abs "elsarticle.tex")))))

(define ((LaTeX-element i) . strs)
  (make-element (style i elsarticle-extras)
                ;; XXX maybe decode-content
                (decode-content strs)))

(define abstract (LaTeX-element "ELSabstract"))
(define author (LaTeX-element "ELSauthor"))
(define address (LaTeX-element "ELSaddress"))
(define email (LaTeX-element "ELSemail"))

(define (frontmatter #:authors as
                     #:abstract a)
  (paragraph (style 'author '())
             (append as (list a))))

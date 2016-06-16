#lang racket/base
(require scribble/doclang
         scribble/core
         racket/file
         (except-in scribble/base author)
         (prefix-in s/b: scribble/base)
         scribble/decode
         "../private/defaults.rkt"
         setup/collects
         scribble/html-properties
         scribble/latex-properties
         scribble/latex-prefix
         racket/stxparam
         net/ftp
         file/gunzip
         (for-syntax racket/base
                     racket/list
                     racket/stxparam-exptime))

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

(define (LaTeX-element i)
  (λ strs
    (make-element (style i elsarticle-extras)
                  ;; XXX maybe decode-content
                  (decode-content strs))))

(define abstract (LaTeX-element "ELSabstract"))
(define author (LaTeX-element "ELSauthor"))
(define address (LaTeX-element "ELSaddress"))
(define email (LaTeX-element "ELSemail"))

(define (frontmatter #:authors as
                     #:abstract a)
  (paragraph (style 'author '())
             (append as (list a))))

#lang racket/base
(require (for-syntax racket/base
                     racket/list
                     racket/stxparam-exptime)
         file/unzip
         net/ftp
         racket/file
         racket/path
         racket/port
         racket/stxparam
         scribble/core
         scribble/decode
         scribble/doclang
         scribble/html-properties
         scribble/latex-prefix
         scribble/latex-properties
         setup/collects
         (only-in net/url string->url get-pure-port)
         (except-in scribble/base author)
         (prefix-in s/b: scribble/base)
         "../private/defaults.rkt")

(module test racket/base)

(provide (except-out (all-from-out scribble/doclang) #%module-begin)
         (all-from-out scribble/base)
         (rename-out [module-begin #%module-begin])
         abstract include-abstract
         authors author
         institute institutes
         email)

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

(define cls-file
  (let ([p (scribble-file "lncs/llncs.cls")])
    (if (file-exists? (collects-relative->path p))
        p
        (downloaded-file "llncs.cls"))))

(define ((post-process) doc)
  (add-defaults doc
                (string->bytes/utf-8 (string-append "\\documentclass{llncs}\n"
                                                    unicode-encoding-packages))
                (scribble-file "lncs/style.tex")
                (list cls-file)
                #f
                #:replacements (hash "scribble-load-replace.tex" (scribble-file "lncs/lncs-load.tex"))))

(define lncs-extras
  (let ([abs (lambda (s)
               (path->collects-relative
                (collection-file-path s "scribble" "lncs")))])
    (list
     (make-css-addition (abs "lncs.css"))
     (make-tex-addition (abs "lncs.tex")))))

(unless (or (not (path? cls-file))
            (file-exists? cls-file))
  (log-error (format "File not found: ~a" cls-file))
  (define file "llncs2e.zip")
  (define cls-file-name (file-name-from-path cls-file))
  (define z (build-path (find-system-path 'temp-dir) file))
  (cond
    [#f
     ;; old site
     (define site "ftp.springernature.com")
     (define path "cs-proceeding/llncs")
     (unless (directory-exists? (find-system-path 'addon-dir))
       (make-directory (find-system-path 'addon-dir)))
     (log-error (format "Downloading via ftp://~a/~a/~a..." site path file))
     (define c (ftp-establish-connection site 21 "anonymous" "user@racket-lang.org"))
     (ftp-cd c path)
     (make-directory* (find-system-path 'temp-dir))
     (ftp-download-file c (find-system-path 'temp-dir) file)
     (ftp-close-connection c)]
    [else
     (define site "https://resource-cms.springernature.com/springer-cms/rest/v1/content/19238648/data/v8")
     (log-error (format "Downloading via ~a..." site))
     (define i (get-pure-port (string->url site) #:redirections 5))
     (call-with-output-file* z #:exists 'truncate (lambda (o) (copy-port i o)))
     (close-input-port i)])
  (define i (open-input-file z))
  (define zipdir (read-zip-directory i))
  (unless (zip-directory-contains? zipdir (path->bytes cls-file-name))
    (error 'lncs "cannot find ~a in archive" cls-file))
  (parameterize ([current-directory (path-only cls-file)])
    (unzip-entry i zipdir (path->bytes cls-file-name)))
  (close-input-port i)
  (delete-file z))

;; ----------------------------------------
;; Abstracts:

(define abstract-style (make-style "abstract" lncs-extras))

(define (abstract . strs)
  (make-nested-flow
   abstract-style
   (decode-flow strs)))

(define (extract-abstract p)
  (unless (part? p)
    (error 'include-abstract "doc binding is not a part: ~e" p))
  (unless (null? (part-parts p))
    (error 'include-abstract "abstract part has sub-parts: ~e" (part-parts p)))
  (when (part-title-content p)
    (error 'include-abstract "abstract part has title content: ~e" (part-title-content p)))
  (part-blocks p))

(define-syntax-rule (include-abstract mp)
  (begin
    (require (only-in mp [doc abstract-doc]))
    (make-nested-flow abstract-style (extract-abstract abstract-doc))))

;; ----------------------------------------
;; Author

(define-syntax (author stx)
  (raise-syntax-error 'author "can only be used inside 'authors'" stx))
(define-syntax (authors stx)
  (syntax-case stx (author)
    [(_ (author . args) ...)
     #`(paragraph
        (style 'author '())
        (make-element (style "LNCSauthor" lncs-extras)
                      (decode-content
                       (list
                        #,@(apply 
                            append
                            (add-between
                             (for/list ([stx (in-list (syntax->list #'(args ...)))])
                               (syntax-case stx ()
                                 [(#:inst string rest ...)
                                  (append (syntax->list #'(rest ...))
                                          (list #'(element (style "LNCSinst" lncs-extras) (decode-content (list string)))))]
                                 [(rest ...)
                                  (syntax->list #'(rest ...))]))
                             (list #'(element (style "LNCSand" lncs-extras) '()))))))))]
    [(_ . rest)
     (raise-syntax-error 'authors "expected a sequence of authors" stx)]))

(define-syntax-parameter email-ok #f)

(define-syntax (institute stx)
  (raise-syntax-error #f "can only be used inside 'institutes'" stx))
(define-syntax (institutes stx)
  (syntax-case stx (author)
    [(_ (inst . args) ...)
     #`(syntax-parameterize 
        ((email-ok #t))
        (paragraph 
         (style 'author '())
         (make-element (style "LNCSinstitutes" lncs-extras)
                       (decode-content
                        (list
                         #,@(apply 
                             append
                             (add-between
                              (for/list ([stx (in-list (syntax->list #'(args ...)))])
                                (syntax-case stx ()
                                  [(rest ...)
                                   (syntax->list #'(rest ...))]))
                              (list #'(element (style "LNCSand" lncs-extras) '())))))))))]
    [(_ . rest)
     (raise-syntax-error 'institutes "expected a sequence of institutes" stx)]))

(define-syntax (email stx)
  (syntax-case stx ()
    [(_ . args)
     (begin
       (unless (syntax-parameter-value #'email-ok)
         (raise-syntax-error 'email "email can appear inside institutes only"))
       #'(make-element (style "LNCSemail" lncs-extras)
                       (decode-content (list . args))))]))

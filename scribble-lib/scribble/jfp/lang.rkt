#lang racket/base
(require (for-syntax racket/base)
         net/ftp
         racket/file
         scribble/doclang
         scribble/jfp
         scribble/latex-prefix
         setup/collects
         (except-in scribble/base author)
         "../private/defaults.rkt")
(provide (except-out (all-from-out scribble/doclang) #%module-begin)
         (all-from-out scribble/jfp)
         (all-from-out scribble/base)
         (rename-out [module-begin #%module-begin]))

(module test racket/base)

;; No options, currently, but keep in case we want to support some:
(define-syntax (module-begin stx)
  (syntax-case* stx () (lambda (a b) (eq? (syntax-e a) (syntax-e b)))
    [(_ id ws . body)
     ;; Skip intraline whitespace to find options:
     (and (string? (syntax-e #'ws))
          (regexp-match? #rx"^ *$" (syntax-e #'ws)))
     #'(module-begin id . body)]
    [(_ id . body)
     #'(#%module-begin id (post-process) () . body)]))

(define cls-file
  (let ([p (scribble-file "jfp/jfp1.cls")])
    (if (file-exists? (collects-relative->path p))
        p
        (downloaded-file "jfp1.cls"))))

(define ((post-process) doc)
  (add-defaults doc
                (string->bytes/utf-8
                 (format "\\documentclass{jfp1}\n~a\\usepackage{times}\n\\usepackage{qcourier}\n~a"
                         unicode-encoding-packages
                         ;; Avoid a conflict with mathabx:
                         "\\let\\amalg\\relax\n"))
                (scribble-file "jfp/style.tex")
                (list cls-file)
                #f
                #:replacements
                (hash "scribble-load-replace.tex" (scribble-file "jfp/replacements.tex"))))

(unless (or (not (path? cls-file))
            (file-exists? cls-file))
  (log-error (format "File not found: ~a" cls-file))
  (define site "ftp.cambridge.org")
  (define path "pub/texarchive/journals/latex/jfp-cls")
  (define file "jfp1.cls")
  (log-error (format "Downloading via ftp://~a/~a/~a..." site path file))
  (with-handlers ([exn:fail? (λ (exn)
                               (define sp (open-output-string))
                               (parameterize ([current-error-port sp])
                                 ((error-display-handler) (exn-message exn) exn))
                               (log-error (format "Failed to download ~a" cls-file))
                               (log-error (get-output-string sp)))])
    (define c (ftp-establish-connection site 21 "anonymous" "user@racket-lang.org"))
    (ftp-cd c path)
    (let-values ([(base name dir?) (split-path cls-file)])
      (make-directory* base)
      (ftp-download-file c base file))
    (ftp-close-connection c)))

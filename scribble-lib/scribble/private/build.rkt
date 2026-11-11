#lang racket/base
;; Loading and rendering documents with the settings that `scribble`'s command
;; line sets.
(require "../xref.rkt"
         "../render.rkt"
         (prefix-in html: "../html-render.rkt"))
(provide mk-doc
         build-docs
         current-lib-mode
         current-doc-binding
         current-doc-load-handler
         current-render-mixin
         current-html
         current-dest-directory
         current-dest-name
         current-info-output-file
         current-info-input-files
         current-xref-input-modules
         current-prefix-file
         current-style-file
         current-style-extra-files
         current-extra-files
         current-redirect
         current-redirect-main
         current-directory-depth
         current-quiet
         helper-file-prefix
         keep-existing-helper-files?
         current-image-prefs)

;; Loading
(define current-lib-mode           (make-parameter #f))
(define current-doc-binding        (make-parameter 'doc))
;; #f, or a `current-load/use-compiled` handler to load documents with (--make)
(define current-doc-load-handler   (make-parameter #f))

;; mk-doc : string [(or/c #f (module-path -> any))] -> any
;; Loads `file` (a `lib` path in lib mode) and returns its document, taken from
;; the `doc` submodule if there is one. With `reload!` (the watcher passes
;; `dynamic-rerequire`), `reload!` (re)loads the chosen module first, and the
;; submodule is used only if it is already declared, rather than loading the
;; module just to check.
(define (mk-doc file [reload! #f])
  (define (go)
    (define mp (if (current-lib-mode)
                   `(lib ,file)
                   `(file ,file)))
    (define sub `(submod ,mp ,(current-doc-binding)))
    (define mp^ (if (module-declared? sub (not reload!)) sub mp))
    (when reload!
      (reload! mp^))
    (dynamic-require mp^ (current-doc-binding)))
  (define handler (current-doc-load-handler))
  (if handler
      (parameterize ([current-load/use-compiled handler])
        (go))
      (go)))

;; Rendering
(define current-render-mixin       (make-parameter html:render-mixin))
(define current-html               (make-parameter #t))
(define current-dest-directory     (make-parameter #f))
(define current-dest-name          (make-parameter #f))
(define current-info-output-file   (make-parameter #f))
(define current-info-input-files   (make-parameter null))
(define current-xref-input-modules (make-parameter null))
(define current-prefix-file        (make-parameter #f))
(define current-style-file         (make-parameter #f))
(define current-style-extra-files  (make-parameter null))
(define current-extra-files        (make-parameter null))
(define current-redirect           (make-parameter #f))
(define current-redirect-main      (make-parameter #f))
(define current-directory-depth    (make-parameter 0))
(define current-quiet              (make-parameter #f))
(define helper-file-prefix         (make-parameter #f))
(define keep-existing-helper-files? (make-parameter #f))
(define current-image-prefs        (make-parameter null)) ; reverse order

(define (build-docs docs files)
  (when (and (current-dest-name)
             ((length files) . > . 1))
    (raise-user-error 'scribble "cannot supply a destination name with multiple inputs"))
  (render docs
          (map (lambda (fn)
                 (let-values ([(base name dir?) (split-path fn)])
                   (or (current-dest-name) name)))
               files)
          #:dest-dir (current-dest-directory)
          #:render-mixin (current-render-mixin)
          #:image-preferences (reverse (current-image-prefs))
          #:prefix-file (current-prefix-file)
          #:style-file (current-style-file)
          #:style-extra-files (reverse (current-style-extra-files))
          #:extra-files (reverse (current-extra-files))
          #:helper-file-prefix (helper-file-prefix)
          #:keep-existing-helper-files? (keep-existing-helper-files?)
          #:redirect (and (current-html) (current-redirect))
          #:redirect-main (and (current-html) (current-redirect-main))
          #:directory-depth (current-directory-depth)
          #:quiet? (current-quiet)
          #:info-in-files (reverse (current-info-input-files))
          #:xrefs (for/list ([mod+id (in-list (reverse (current-xref-input-modules)))])
                    (let* ([get-xref (dynamic-require (car mod+id) (cdr mod+id))]
                           [xr (get-xref)])
                      (unless (xref? xr)
                        (raise-user-error
                         'scribble "result from `~s' of `~s' is not an xref: ~e"
                         (cdr mod+id) (car mod+id) xr))
                      xr))
          #:info-out-file (current-info-output-file)))

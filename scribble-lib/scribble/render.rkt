#lang racket/base

(require racket/class
         racket/contract/base
         racket/file
         (prefix-in html: "html-render.rkt")
         "base-render.rkt"
         "core.rkt"
         "xref.rkt")

(provide (contract-out [render
                        (((listof part?) (listof path-string?))
                         (#:render-mixin (class? . -> . class?)
                                         #:dest-dir (or/c #f path-string?)
                                         #:helper-file-prefix (or/c #f string?)
                                         #:keep-existing-helper-files? any/c
                                         #:prefix-file (or/c #f path-string?)
                                         #:style-file (or/c #f path-string?)
                                         #:style-extra-files (listof path-string?)
                                         #:extra-files (listof path-string?)
                                         #:image-preferences (listof (or/c 'ps 'pdf 'png 'svg 'gif))
                                         #:redirect (or/c #f string?)
                                         #:redirect-main (or/c #f string?)
                                         #:directory-depth exact-nonnegative-integer?
                                         #:xrefs (listof xref?)
                                         #:info-in-files (listof path-string?)
                                         #:info-out-file (or/c #f path-string?)
                                         #:quiet? any/c
                                         #:warn-undefined? any/c)
                         . ->* .
                         void?)]))

(define (render docs
                names
                #:render-mixin [render-mixin html:render-mixin]
                #:dest-dir [dest-dir #f]
                #:helper-file-prefix [helper-file-prefix #f]
                #:keep-existing-helper-files? [keep-existing-helper-files? #f]
                #:prefix-file [prefix-file #f]
                #:style-file [style-file #f]
                #:style-extra-files [style-extra-files null]
                #:extra-files [extra-files null]
                #:image-preferences [image-preferences null]
                #:redirect [redirect #f]
                #:redirect-main [redirect-main #f]
                #:directory-depth [directory-depth 0]
                #:xrefs [xrefs null]
                #:info-in-files [info-input-files null]
                #:info-out-file [info-output-file #f]
                #:quiet? [quiet? #t]
                #:warn-undefined? [warn-undefined? (not quiet?)])
  (when dest-dir (make-directory* dest-dir))
  (define renderer
    (new (render-mixin render%)
         [dest-dir dest-dir]
         [prefix-file prefix-file]
         [style-file style-file]
         [style-extra-files style-extra-files]
         [extra-files extra-files]
         [image-preferences image-preferences]
         [helper-file-prefix helper-file-prefix]
         [keep-existing-helper-files? keep-existing-helper-files?]))
  (when redirect
    (send renderer set-external-tag-path redirect))
  (when redirect-main
    (send renderer set-external-root-url redirect-main))
  (unless (zero? directory-depth)
    (send renderer set-directory-depth directory-depth))
  (unless quiet?
    (send renderer report-output!))
  (define fns
    (map (lambda (fn)
           (let-values ([(base name dir?) (split-path fn)])
             (let ([fn (path-replace-suffix name (send renderer get-suffix))])
               (if dest-dir
                   (build-path dest-dir fn)
                   fn))))
         names))
  (define fp (send renderer traverse docs fns))
  (define info (send renderer collect docs fns fp))
  (for ([file (in-list info-input-files)])
    (let ([s (with-input-from-file file read)]) (send renderer deserialize-info s info)))
  (for ([xr (in-list xrefs)])
    (xref-transfer-info renderer info xr))
  (let ([r-info (send renderer resolve docs fns info)])
    (send renderer render docs fns r-info)
    (when info-output-file
      (let ([s (send renderer serialize-info r-info)])
        (with-output-to-file info-output-file #:exists 'truncate/replace (lambda () (write s)))))
    (when warn-undefined?
      (let ([undef (send renderer get-undefined r-info)])
        (unless (null? undef)
          (eprintf "Warning: some cross references may be broken due to undefined tags:\n")
          (for ([t (in-list undef)])
            (eprintf " ~s\n" t))))))
  (void))

#lang racket/base
(require racket/cmdline
         racket/runtime-path
         racket/pretty
         racket/file
         racket/match
         scribble/render
         xml
         (prefix-in footnote1: "footnote1.scrbl")
         (prefix-in footnote2: "footnote2.scrbl"))

;; Renders documents to HTML, extracts filtered versions
;; of the HTML that keeps only relevant things involving layout
;; and styles, and checks whether that matches an expected content.
;; The expected content may need to be reset if Scribble changes.

(define-runtime-path footnote1.rktl "footnote1.rktl")
(define-runtime-path footnote2.rktl "footnote2.rktl")

(define show? #f)
(define save? #f)

(command-line
 #:once-each
 [("--show") "show filetered output"
             (set! show? #t)]
 [("--save") "save output for regression test"
             (set! save? #t)])

(define (check doc expect-file)
  (define dir (make-temporary-directory))
  (render (list doc)
          (list "doc")
          #:dest-dir dir)
  (define raw-html
    (call-with-input-file*
     (build-path dir "doc.html")
     (lambda (in)
       (xml->xexpr (document-element (read-xml in))))))
  (delete-directory/files dir)

  (define keep-with-attrs '(div span a))
  (define keeps '(html body section table tr td p blockquote h2))
  (define skip-attr-classes '("tocset"  "versionbox"))
  (define skip-content-classes '("versionNoNav"))

  (define (filter html)
    (define (filter-body htmls)
      (for/list ([html (in-list htmls)]
                 #:do [(define new (filter html))]
                 #:when new)
        new))
    (define (filter-attrs keys+vals)
      (for/list ([key+val (in-list keys+vals)]
                 #:do [(define key (car key+val))
                       (define val (cadr key+val))]
                 #:when (member key '(class style href name)))
        (list key val)))
    (define (has-skip-class? keys+vals skip-classes)
      (for/or ([key+val (in-list keys+vals)])
        (define key (car key+val))
        (define val (cadr key+val))
        (and (eq? key 'class)
             (member val skip-classes))))
    (define (has-skip-attr-class? keys+vals)
      (has-skip-class? keys+vals skip-attr-classes))
    (define (has-skip-content-class? keys+vals)
      (has-skip-class? keys+vals skip-content-classes))
    (match html
      [`(,tag ,keys+vals ,body ...)
       (cond
         [(member tag keep-with-attrs)
          (cond
            [(has-skip-attr-class? keys+vals)
             `(,tag ,@(filter-body body))]
            [(has-skip-content-class? keys+vals)
             `(,tag ,(filter-attrs keys+vals))]
            [else
             `(,tag ,(filter-attrs keys+vals) ,@(filter-body body))])]
         [(member tag keeps)
          `(,tag ,@(filter-body body))]
         [else
          tag])]
      [_ html]))

  (define html (filter raw-html))

  (when show?
    (pretty-write html))

  (when save?
    (call-with-output-file*
     expect-file
     #:exists 'truncate
     (lambda (o)
       (pretty-write html o))))

  (define expect (call-with-input-file* expect-file read))

  (unless (equal? html expect)
    (error "failed" expect-file)))

(check footnote1:doc footnote1.rktl)
(check footnote2:doc footnote2.rktl)

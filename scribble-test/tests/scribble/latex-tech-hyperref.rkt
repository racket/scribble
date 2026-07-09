#lang racket/base

(require racket/file
         rackunit
         scribble/core
         scribble/latex-render
         scribble/manual
         scribble/render)

(define (latex-render-string doc)
  (dynamic-wind
   (λ() (render (list doc)
                (list "example-for-latex-tech-hyperref-test")
                #:render-mixin render-mixin))
   (λ() (file->string "example-for-latex-tech-hyperref-test.tex"))
   (λ() (delete-file "example-for-latex-tech-hyperref-test.tex"))))

; Regression test: when @tech is used with #:tag-prefixes, the resolved
; destination tag must be used for \hyperref rather than the raw link-element
; tag, so that \hyperref and \label refer to the same anchor.
(test-case "tech hyperref tag matches deftech label tag when using tag-prefixes"
  (define str
    (latex-render-string
     (part "book:"
           '((part "top"))
           '("Doc")
           (style #f null)
           null
           null
           (list
            (part "chp1:"
                  '((part "chp1"))
                  '("Chapter One")
                  (style #f null)
                  null
                  (list (paragraph (style #f null) (list (deftech "widget"))))
                  null)
            (part #f
                  '((part "sec2"))
                  '("Section Two")
                  (style #f null)
                  null
                  (list (paragraph (style #f null)
                                   (list (tech #:tag-prefixes '("book:" "chp1:") "widget"))))
                  null)))))

  (define label-m
    (regexp-match #rx"\\\\label\\{(t:[a-z0-9:_]+)\\}\\\\textit\\{widget\\}" str))
  (define href-m
    (regexp-match #rx"\\\\hyperref\\[(t:[a-z0-9:_]+)\\]" str))

  (check-not-false (and label-m href-m) "both \\label and \\hyperref for tech term must appear")
  (check-equal? (cadr label-m) (cadr href-m)
                "\\hyperref tag must match \\label tag for tech term"))

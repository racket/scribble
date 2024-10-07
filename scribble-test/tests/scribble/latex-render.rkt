#lang racket/base

(require racket/file
         racket/string
         rackunit
         scribble/core
         scribble/latex-render
         scribble/render)

(define (latex-render-string doc)
  (dynamic-wind
   (λ() (render (list doc)
                (list "example-for-latex-render-test")
                #:render-mixin render-mixin))
   (λ() (file->string "example-for-latex-render-test.tex"))
   (λ() (delete-file "example-for-latex-render-test.tex"))))

(test-case "guillemet"
  (define str
    (latex-render-string
     (part #f
           null
           (list "Render")
           (style #f null)
           null
           (list (paragraph (style #f null) "The «content»."))
           null)))

  (check-false (string-contains? str "\\guillemotleftcontent"))
  (check string-contains? str "\\guillemotleft")
  (check string-contains? str "content")
  (check string-contains? str "\\guillemotright"))

(test-case "vdots and cdots"
  (define str
    (latex-render-string
     (part #f
           null
           (list "Render")
           (style #f null)
           null
           (list (paragraph (style #f null) "The ⋮tall⋮ and the ⋯wide⋯."))
           null)))

  (check-false (string-contains? str "\\vdotstall"))
  (check-false (string-contains? str "\\cdotswide"))
  (check string-contains? str "\\vdots")
  (check string-contains? str "tall")
  (check string-contains? str "\\cdots")
  (check string-contains? str "wide"))


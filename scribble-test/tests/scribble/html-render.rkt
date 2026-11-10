#lang racket/base

(require racket/class
         racket/file
         rackunit
         scribble/base
         scribble/base-render
         scribble/core
         (only-in scribble/decode decode)
         scribble/html-properties
         (prefix-in html: scribble/html-render)
         (only-in scribble/render render))

(define renderer
  (new (html:render-mixin render%)
       [dest-dir (find-system-path 'temp-dir)]))

(define (render-content content)
  (send renderer render-content content #f #f))

(test-case "semantic text styles"
  (check-equal?
   (render-content (emph "emphasized"))
   '((em ((class "emph")) "emphasized")))

  (check-equal?
   (render-content (emph "outer " (emph "inner")))
   '((em ((class "emph")) "outer "
         (em ((class "emph")) "inner"))))

  (check-equal?
   (render-content
    (elem #:style
          (make-style 'emph
                      (list (attributes '((data-test . "value")))))
          "styled emphasis"))
   '((em ((class "emph") (data-test "value")) "styled emphasis")))

  (check-equal?
   (render-content (superscript "superscript"))
   '((sup ((style "vertical-align: super; font-size: 80%; line-height: inherit"))
          "superscript")))

  (check-equal?
   (render-content (subscript "subscript"))
   '((sub ((style "vertical-align: sub; font-size: 80%; line-height: inherit"))
          "subscript")))

  (check-equal?
   (render-content
    (elem #:style
          (make-style 'emph (list (alt-tag "i")))
          "alternate tag"))
   '((i ((class "emph")) "alternate tag")))

  (check-equal?
   (render-content
    (elem #:style
          (make-style 'emph
                      (list (target-url "https://example.com")))
          "linked"))
   '((a ((href "https://example.com") (class "emph")) "linked")))

  (check-equal?
   (render-content (elem #:style "emph" "custom style"))
   '((span ((class "emph")) "custom style"))))

(define (render-title version date)
  (define dest (make-temporary-directory))
  (dynamic-wind
    void
    (lambda ()
      (define doc
        (decode (list (title #:version version #:date date "Example"))))
      (render (list doc) (list "example")
              #:render-mixin html:render-mixin
              #:dest-dir dest)
      (file->string (build-path dest "example.html")))
    (lambda () (delete-directory/files dest))))

(test-case "document date in the HTML version box"
  (define date "2026-11-09")
  (define date-element
    #rx"<div class=\"document-date\">2026-11-09</div>")
  (define version-class #rx"class=\"versionNoNav\"")
  (define date-only (render-title "" date))

  (check-true
   (regexp-match?
    #px"class=\"versionNoNav\">1\\.2\\.3</span>\\s*<div class=\"document-date\">2026-11-09</div>"
    (render-title "1.2.3" date)))
  (check-true (regexp-match? date-element date-only))
  (check-false (regexp-match? version-class date-only))

  (for ([absent-date (in-list (list #f ""))])
    (define version-only (render-title "1.2.3" absent-date))
    (check-true (regexp-match? version-class version-only))
    (check-false (regexp-match? #rx"class=\"document-date\"" version-only)))

  (check-false
   (regexp-match? #rx"class=\"versionbox\"" (render-title "" ""))))

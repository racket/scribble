#lang racket/base

(require racket/class
         rackunit
         scribble/base
         scribble/base-render
         scribble/core
         scribble/html-properties
         (prefix-in html: scribble/html-render))

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

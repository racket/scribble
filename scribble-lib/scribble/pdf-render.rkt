#lang racket/base

(require "private/indirect-renderer.rkt" "private/run-pdflatex.rkt"
         (prefix-in latex: "latex-render.rkt"))

(provide render-mixin
         dvi-render-mixin
         xelatex-render-mixin
         lualatex-render-mixin)

(define render-mixin
  (make-indirect-renderer-mixin
   (λ (%) (latex:render-mixin % #:image-mode 'pdf)) #".tex" #".pdf"
   run-pdflatex))

(define dvi-render-mixin
  (make-indirect-renderer-mixin
   (λ (%) (latex:render-mixin % #:image-mode 'ps)) #".tex" #".pdf"
   run-dvipdf-latex))

(define xelatex-render-mixin
  (make-indirect-renderer-mixin
   (λ (%) (latex:render-mixin % #:image-mode 'pdf)) #".tex" #".pdf"
   run-xelatex))

(define lualatex-render-mixin
  (make-indirect-renderer-mixin
   (λ (%) (latex:render-mixin % #:image-mode 'pdf)) #".tex" #".pdf"
   run-lualatex))

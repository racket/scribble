#lang racket/base
(require racket/runtime-path
         scribble/latex-properties)

(provide book-index-style-properties)

(define-runtime-path book-index.tex "book-index.tex")

(define book-index-style-properties
  (list
   (tex-addition book-index.tex)
   'enable-index-merge))

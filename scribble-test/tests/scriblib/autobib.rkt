#lang racket

(require scriblib/autobib)

(let ()
  (define-cite cite citet gen-bib)
  cite citet gen-bib
  (void))

(let ()
  (define-cite cite citet gen-bib
    #:cite-author cite-author
    #:cite-year cite-year)
    cite citet gen-bib cite-author cite-year
  (void))

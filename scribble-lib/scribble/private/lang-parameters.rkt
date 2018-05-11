#lang racket/base

(provide (all-defined-out))

;; Some latex formats have different requirements on how
;;   figures, citations, etc. are displayed. This allows different
;;   scribble langs to handle them.

;; `Figure` string that appears in front of a figure caption
(define default-figure-label-text (make-parameter "Figure"))

;; Seperator string between figure counter and caption
(define default-figure-label-sep (make-parameter ": "))

;; Style for the figure caption
(define default-figure-caption-style (make-parameter #f))

;; Style for the number in the figure counter
(define default-figure-counter-style (make-parameter #f))

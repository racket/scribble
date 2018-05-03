#lang racket/base

(provide (all-defined-out))

;; Some latex formats have different requirements on how
;;   figures, citations, etc. are displayed. This allows different
;;   scribble langs to handle them.

(define default-label-text (make-parameter "Figure"))
(define default-label-sep (make-parameter ": "))
(define default-caption-style (make-parameter #f))
(define default-counter-style (make-parameter #f))

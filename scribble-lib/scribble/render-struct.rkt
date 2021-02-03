#lang racket/base

(require racket/serialize)
(provide (struct-out mobile-root))

(define-serializable-struct mobile-root (path) 
  #:mutable 
  #:transparent)

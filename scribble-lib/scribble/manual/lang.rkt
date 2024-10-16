#lang racket/base
(require scribble/doclang
         scribble/html-properties
         scribble/manual
         "../private/manual-defaults.rkt")
(provide (except-out (all-from-out scribble/doclang) #%module-begin)
         (all-from-out scribble/manual)
         (rename-out [module-begin #%module-begin])
         manual-doc-style)

(define-syntax-rule (module-begin id . body)
  (#%module-begin id post-process () . body))

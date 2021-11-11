#lang racket/base
(require "private/doc-begin.rkt"
         (for-syntax racket/base))

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [*module-begin #%module-begin]))

;; Module wrapper ----------------------------------------

(define-syntax (*module-begin stx)
  (syntax-case stx ()
    [(_ id post-process exprs . body)
     #'(#%module-begin
        (module configure-runtime racket/base (require scribble/base/lang/configure-runtime))
        (doc-begin id post-process exprs . body))]))

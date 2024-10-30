#lang racket/base

(provide valid-blueboxes-info?)

(require racket/contract/base
         scribble/core)

(define valid-blueboxes-info?
  (hash/c
   tag?
   (listof (cons/c exact-nonnegative-integer?
                   exact-nonnegative-integer?))
   #:flat? #t))

#lang racket/base

(provide valid-blueboxes-info?)

(require scribble/core racket/contract/base)

(define valid-blueboxes-info?
  (hash/c
   tag?
   (listof (cons/c exact-nonnegative-integer?
                   exact-nonnegative-integer?))
   #:flat? #t))

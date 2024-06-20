#lang racket/base

(provide valid-signature-box-info?)

(require scribble/core racket/contract/base)

(define valid-signature-box-info?
  (hash/c
   tag?
   (listof (cons/c exact-nonnegative-integer?
                   exact-nonnegative-integer?))
   #:flat? #t))

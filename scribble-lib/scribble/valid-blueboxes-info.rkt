#lang racket/base

(provide valid-blueboxes-info?)

(require scribble/core racket/contract/base)

(define valid-blueboxes-info?
  (hash/c
   tag?
   (listof (cons/dc [hd exact-nonnegative-integer?]
                    [tl (hd) (and/c exact-nonnegative-integer?
                                    (>/c hd))]
                    #:flat))
   #:flat? #t))

#lang racket/base
;; Dependencies that are only required inside submodules.
(provide h)
(define h 1)
(module extra racket/base
  (module inner racket/base
    (require "subs-nested-dep.rkt"))
  (require "subs-extra-dep.rkt" (submod "." inner))
  (provide e)
  (define e 2))
;; Not required by anything else, so its imports are not dependencies.
(module+ test
  (require (submod ".." extra) "subs-test-only.rkt"))

#lang racket/base
;; Dependencies that are only required inside submodules.
(provide h)
(define h 1)
(module extra racket/base
  (require "subs-extra-dep.rkt")
  (provide e)
  (define e 2)
  (module inner racket/base
    (require "subs-nested-dep.rkt")))
(module+ test
  (require (submod ".." extra) "subs-test-only.rkt"))

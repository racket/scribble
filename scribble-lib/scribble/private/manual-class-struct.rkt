#lang racket/base
(require racket/serialize)

(provide (all-defined-out))

(define-serializable-struct/versions cls/intf 1
  (name-element app-mixins super intfs intfs/no-inherit methods)
  ([0 (lambda (name-element app-mixins super intfs methods)
        (cls/intf name-element app-mixins super intfs null methods))
      (lambda () (error "no cyclic cls/intf"))])
  #:transparent)

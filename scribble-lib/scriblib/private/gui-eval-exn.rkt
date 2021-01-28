#lang racket/base
(require mzlib/serialize)

(define-serializable-struct gui-exn (message))

(provide (struct-out gui-exn))

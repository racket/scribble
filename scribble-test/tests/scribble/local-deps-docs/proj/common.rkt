#lang racket/base
(require scribble/manual)
(provide note-it stamp)
(define (note-it s) (italic s))
(define-syntax-rule (stamp) "STAMP-V1")

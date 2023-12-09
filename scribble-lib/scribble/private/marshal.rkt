#lang racket/base

(provide check-marshalable)

(require racket/fasl
         racket/match)

;; check-marshalable : any/c -> (or/c #f any/c)
(define (check-marshalable v)
  (match (call/cc (λ (return)
                    (s-exp->fasl
                     v
                     #:handle-fail (λ (v) (return `(failed ,v))))))
    [`(failed ,v) v]
    [_ #f]))

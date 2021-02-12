#lang racket/base
(require (for-syntax racket/base))

(provide (all-defined-out))

(define (f) 10)
(define (g x y) (void))
(define (h x #:y y) (void))
(define (i x #:y [y #f]) (void))
(define (j) (void))

(define-syntax-rule (m x) 'x)

(define-syntax n (lambda (stx) #`(quote #,stx)))

(define p (make-parameter 10))
(define q (make-parameter #f))

(struct pt (x y)
  #:extra-constructor-name make-pt)
(struct pn (x y))

(define v 10)

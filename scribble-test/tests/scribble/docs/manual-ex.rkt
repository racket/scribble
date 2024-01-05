#lang racket/base
(require (for-syntax racket/base)
         racket/fixnum
         racket/flonum)

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

(define-struct pt (x y))
(struct pn (x y))
(struct counter (name [count #:mutable]))

(define v 10)

(define val:flvector (flvector 1.0 2.0))
(define val:fxvector (fxvector 1 2))
(define val:extflonum 1.0t0)
(define val:kw '#:foo)
(define val:list '(1 2 3 4))
(define val:vector #(1 2 3 4))
(define val:param (make-parameter 'foo))

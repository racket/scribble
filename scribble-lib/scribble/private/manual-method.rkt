#lang racket/base
(require "../struct.rkt"
         "../search.rkt"
         "../scheme.rkt"
         "../basic.rkt"
         "manual-scheme.rkt"
         (for-syntax racket/base))

(provide ;; public:
         method xmethod)
; XXX unknown contracts
(provide *method **method
         method-tag
         constructor-tag
         name-this-object)

(define-syntax-rule (method class/interface method-name)
  (*method 'method-name (quote-syntax class/interface)))

(define-syntax-rule (xmethod class/intf-id method-id)
  (elem (method class/intf-id method-id) " in " (racket class/intf-id)))

(define (*method sym id
                 #:defn? [defn? #f])
  (**method sym id #:defn? defn?))

(define (**method sym id/tag
                  #:defn? [defn? #f])
  (define content (list (symbol->string sym)))
  (define (mk tag)
    (make-element symbol-color
                  (list (make-link-element (if defn?
                                               value-def-color 
                                               value-link-color)
                                           content
                                           (method-tag tag sym)))))
  (if (identifier? id/tag)
      (make-delayed-element
       (λ (ren p ri)
         (define tag (find-scheme-tag p ri id/tag #f))
         (if tag (list (mk tag)) content))
       (λ () (car content))
       (λ () (car content)))
      (mk id/tag)))

(define (method-tag vtag sym)
  (list 'meth (list (cadr vtag) sym)))

(define (constructor-tag vtag)
  (list 'constructor (cadr vtag)))

(define (name-this-object type-sym)
  (to-element
   (string->symbol
    (regexp-replace
     #rx"(%|<%>|-mixin)$"
     (format "_a~a-~s"
             (if (member (string-ref (symbol->string type-sym) 0)
                         '(#\a #\e #\i #\o #\u))
               "n"
               "")
             type-sym)
     ""))))

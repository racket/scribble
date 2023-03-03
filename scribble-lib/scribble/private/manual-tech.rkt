#lang racket/base
(require racket/contract/base
         "../decode.rkt"
         "../struct.rkt"
         (only-in "../core.rkt" style)
         "manual-utils.rkt"
         "manual-style.rkt")

(provide/contract
 [deftech (() (#:normalize? any/c
               #:style? any/c
               #:key (or/c string? #f))
           #:rest (listof pre-content?) . ->* . element?)]
 [tech (() 
        (#:doc (or/c module-path? #f) 
         #:tag-prefixes (or/c (listof string?) #f) 
         #:key (or/c string? #f)
         #:normalize? any/c
         #:indirect? any/c)
        #:rest (listof pre-content?) 
        . ->* . element?)]
 [techlink (() 
            (#:doc (or/c module-path? #f) 
             #:tag-prefixes (or/c (listof string?) #f) 
             #:key (or/c string? #f)
             #:normalize? any/c
             #:indirect? any/c)
            #:rest (listof pre-content?) 
            . ->* . element?)])

(define (*tech make-elem style doc prefix s key normalize?)
  (let* ([c (decode-content s)]
         [s (or key (content->string c))]
         [s (if normalize?
                (let* ([s (string-foldcase s)]
                       [s (regexp-replace #rx"ies$" s "y")]
                       [s (regexp-replace #rx"s$" s "")]
                       [s (regexp-replace* #px"[-\\s]+" s " ")])
                  s)
                s)]
         [s (datum-intern-literal s)])
    (make-elem style c (list 'tech (doc-prefix doc prefix s)))))

(define (deftech #:style? [style? #t] 
          #:normalize? [normalize? #t] 
          #:key [key #f]
          . s)
  (define e
    (if style?
        (apply defterm s)
        (make-element #f (decode-content s))))
  (define t (*tech make-target-element #f #f #f (list e) key normalize?))
  (make-index-element #f
                      (list t)
                      (target-element-tag t)
                      (list (datum-intern-literal
                             (clean-up-index-string (element->string e))))
                      (list e)
                      'tech))

(define (tech #:doc [doc #f]
              #:tag-prefixes [prefix #f]
              #:key [key #f]
              #:normalize? [normalize? #t]
              #:indirect? [indirect? #f]
              . s)
  (*tech (lambda (sty c tag)
           (make-link-element
            (if indirect?
                (style sty '(indirect-link))
                sty)
            (list (make-element "techinside" c))
            tag))
         "techoutside"
         doc prefix s key
         normalize?))

(define (techlink #:doc [doc #f] 
                  #:tag-prefixes [prefix #f] 
                  #:key [key #f] 
                  #:normalize? [normalize? #t]
                  #:indirect? [indirect? #f]
                  . s)
  (*tech (lambda (sty c tag)
           (make-link-element
            (if indirect?
                (style sty '(indirect-link))
                sty)
            c
            tag))
         #f
         doc prefix s key
         normalize?))

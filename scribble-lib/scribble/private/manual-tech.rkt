#lang racket/base
(require racket/contract/base
         "../decode.rkt"
         "../struct.rkt"
         "manual-utils.rkt"
         "manual-style.rkt")

(provide/contract
 [deftech (() (#:normalize? any/c
               #:style? any/c
               #:key (or/c string? #f))
           #:rest (listof pre-content?) . ->* . element?)]
 [tech (() 
        (#:doc (or/c module-path? false/c) 
         #:tag-prefixes (or/c (listof string?) false/c) 
         #:key (or/c string? #f)
         #:normalize? any/c)
        #:rest (listof pre-content?) 
        . ->* . element?)]
 [techlink (() 
            (#:doc (or/c module-path? false/c) 
             #:tag-prefixes (or/c (listof string?) false/c) 
             #:key (or/c string? #f)
             #:normalize? any/c)
            #:rest (listof pre-content?) 
            . ->* . element?)])

(define (*tech make-elem style doc prefix s key normalize?)
  (let* ([c (decode-content s)]
         [s (or key (content->string c))]
         [s (if normalize? (normalize s) s)]
         [s (datum-intern-literal s)])
    (make-elem style c (list 'tech (doc-prefix doc prefix s)))))

(define (deftech #:style? [style? #t] 
          #:normalize? [normalize? #t] 
          #:key [key #f]
          . s)
  (let* ([e (if style?
                (apply defterm s)
                (make-element #f (decode-content s)))]
         [t (*tech make-target-element #f #f #f (list e) key normalize?)])
    (make-index-element #f
                        (list t)
                        (target-element-tag t)
                        (list (datum-intern-literal
                               (clean-up-index-string (element->string e))))
                        (list e)
                        'tech)))

(define (tech #:doc [doc #f] 
              #:tag-prefixes [prefix #f] 
              #:key [key #f] 
              #:normalize? [normalize? #t] 
              . s)
  (*tech (lambda (style c tag)
           (make-link-element
            style
            (list (make-element "techinside" c))
            tag))
         "techoutside"
         doc prefix s key
         normalize?))

(define (techlink #:doc [doc #f] 
                  #:tag-prefixes [prefix #f] 
                  #:key [key #f] 
                  #:normalize? [normalize? #t] 
                  . s)
  (*tech make-link-element #f doc prefix s key normalize?))

(define (normalize raw-string)
  (define folded (string-foldcase raw-string))
  ;; This logic was derived from the rules for singular and plural words
  ;; described here: https://www.ef.com/wwen/english-resources/english-grammar/singular-and-plural-nouns/
  (define singular
    (cond
      [(regexp-match? #rx"ies$" folded) (regexp-replace #rx"ies$" folded "y")]
      [(regexp-match? #rx"(s|x|z|ch|sh)es$" folded)
       (regexp-replace #rx"es$" folded "")]
      [(regexp-match? #rx"[^s]s%" folded) (regexp-replace #rx"s$" folded "")]
      [else folded]))
  (regexp-replace* #px"[-\\s]+" singular " "))

#lang racket/base
(require "../decode.rkt"
         "../struct.rkt"
         "../scheme.rkt"
         "../search.rkt"
         "../basic.rkt"
         (only-in "../core.rkt" style style-properties)
         "manual-style.rkt"
         "manual-utils.rkt" ;; used via datum->syntax
         "on-demand.rkt"
         (for-syntax racket/base)
         (for-label racket/base)
         racket/deprecation)

(provide racketblock RACKETBLOCK racketblock/form
         racketblock0 RACKETBLOCK0 racketblock0/form
         racketresultblock racketresultblock0
         RACKETRESULTBLOCK RACKETRESULTBLOCK0
         racketblockelem
         racketinput RACKETINPUT
         racketinput0 RACKETINPUT0
         racketmod
         racketmod0
         racket RACKET racket/form racketresult racketid 
         racketmodname
         racketmodlink indexed-racket
         racketlink
         schemeblock
         SCHEMEBLOCK
         schemeblock/form
         schemeblock0
         SCHEMEBLOCK0
         schemeblock0/form
         schemeblockelem
         schemeinput
         schememod
         scheme
         SCHEME
         scheme/form
         schemeresult
         schemeid
         schememodname
         schememodlink
         indexed-scheme)


(define-deprecated-alias schemeblock racketblock)
(define-deprecated-alias SCHEMEBLOCK RACKETBLOCK)
(define-deprecated-alias schemeblock/form racketblock/form)
(define-deprecated-alias schemeblock0 racketblock0)
(define-deprecated-alias SCHEMEBLOCK0 RACKETBLOCK0)
(define-deprecated-alias schemeblock0/form racketblock0/form)
(define-deprecated-alias schemeblockelem racketblockelem)
(define-deprecated-alias schemeinput racketinput)
(define-deprecated-alias schememod racketmod)
(define-deprecated-alias scheme racket)
(define-deprecated-alias SCHEME RACKET)
(define-deprecated-alias scheme/form racket/form)
(define-deprecated-alias schemeresult racketresult)
(define-deprecated-alias schemeid racketid)
(define-deprecated-alias schememodname racketmodname)
(define-deprecated-alias schememodlink racketmodlink)
(define-deprecated-alias indexed-scheme indexed-racket)
(define-deprecated-alias schemelink racketlink)


(define-code racketblock0 to-paragraph)
(define-code racketblock to-block-paragraph)
(define-code RACKETBLOCK to-block-paragraph UNSYNTAX)
(define-code RACKETBLOCK0 to-paragraph UNSYNTAX)

(define (to-block-paragraph v)
  (code-inset (to-paragraph v)))

(define (to-result-paragraph v)
  (to-paragraph v 
                #:color? #f 
                #:wrap-elem
                (lambda (e) (make-element result-color e))))
(define (to-result-paragraph/prefix a b c)
  (define to-paragraph (to-paragraph/prefix a b c))
  (lambda (v)
    (to-paragraph v 
                  #:color? #f 
                  #:wrap-elem
                  (lambda (e) (make-element result-color e)))))

(define-code racketresultblock0 to-result-paragraph)
(define-code racketresultblock (to-result-paragraph/prefix (hspace 2) (hspace 2) ""))
(define-code RACKETRESULTBLOCK (to-result-paragraph/prefix (hspace 2) (hspace 2) "")
  UNSYNTAX)
(define-code RACKETRESULTBLOCK0 to-result-paragraph UNSYNTAX)

(define interaction-prompt (make-element 'tt (list "> " )))
(define-code racketinput to-input-paragraph/inset)
(define-code RACKETINPUT to-input-paragraph/inset)
(define-code racketinput0 to-input-paragraph)
(define-code RACKETINPUT0 to-input-paragraph)

(define to-input-paragraph
  (to-paragraph/prefix
   (make-element #f interaction-prompt)
   (hspace 2)
   ""))
  
(define (to-input-paragraph/inset v)
  (code-inset (to-input-paragraph v)))

(define-syntax (racketmod0 stx)
  (syntax-case stx ()
    [(_ #:file filename #:escape unsyntax-id lang rest ...)
     (with-syntax ([modtag (datum->syntax
                            #'here
                            (list #'unsyntax-id
                                  `(make-element
                                    #f
                                    (list (hash-lang)
                                          spacer
                                          ,(if (identifier? #'lang)
                                               `(as-modname-link
                                                 ',#'lang
                                                 (to-element ',#'lang)
                                                 #f)
                                               #'(racket lang)))))
                            #'lang)])
       (if (syntax-e #'filename)
           (quasisyntax/loc stx
             (filebox
              filename
              #,(syntax/loc stx (racketblock0 #:escape unsyntax-id modtag rest ...))))
           (syntax/loc stx (racketblock0 #:escape unsyntax-id modtag rest ...))))]
    [(_ #:file filename lang rest ...)
     (syntax/loc stx (racketmod0 #:file filename #:escape unsyntax lang rest ...))]
    [(_ lang rest ...)
     (syntax/loc stx (racketmod0 #:file #f lang rest ...))]))

(define-syntax-rule (racketmod rest ...)
  (code-inset (racketmod0 rest ...)))

(define (to-element/result s)
  (make-element result-color (list (to-element/no-color s))))
(define (to-element/id s)
  (make-element symbol-color (list (to-element/no-color s))))
(define (to-element/no-escapes s)
  (to-element s #:escapes? #f))

(define-syntax (keep-s-expr stx)
  (syntax-case stx (quote)
    [(_ ctx '#t #(src line col pos 5))
     #'(make-long-boolean #t)]
    [(_ ctx '#f #(src line col pos 6))
     #'(make-long-boolean #f)]
    [(_ ctx s srcloc)
     (let ([sv (syntax-e
                (syntax-case #'s (quote)
                  [(quote s) #'s]
                  [_ #'s]))])
       (if (or (number? sv)
               (boolean? sv)
               (and (pair? sv)
                    (identifier? (car sv))
                    (or (free-identifier=? #'cons (car sv))
                        (free-identifier=? #'list (car sv)))))
           ;; We know that the context is irrelvant
           #'s
           ;; Context may be relevant:
           #'(*keep-s-expr s ctx)))]))
(define (*keep-s-expr s ctx)
  (if (symbol? s)
    (make-just-context s ctx)
    s))

(define (add-sq-prop s name val)
  (if (eq? name 'paren-shape)
    (make-shaped-parens s val)
    s))

(define-code racketblockelem to-element)

(define-code racket to-element unsyntax keep-s-expr add-sq-prop)
(define-code RACKET to-element UNSYNTAX keep-s-expr add-sq-prop)
(define-code racketresult to-element/result unsyntax keep-s-expr add-sq-prop)
(define-code racketid to-element/id unsyntax keep-s-expr add-sq-prop)
(define-code *racketmodname to-element/no-escapes unsyntax keep-s-expr add-sq-prop)

(define-syntax (**racketmodname stx)
  (syntax-case stx ()
    [(_ form)
     (let ([stx #'form])
       #`(*racketmodname
          ;; We want to remove lexical context from identifiers
          ;; that correspond to module names, but keep context
          ;; for `lib' or `planet' (which are rarely used)
          #,(cond
              [(identifier? stx) (datum->syntax #f (syntax-e stx) stx stx)]
              [(and (pair? (syntax-e stx))
                    (memq (syntax-e (car (syntax-e stx))) '(lib planet file)))
               (define s (car (syntax-e stx)))
               (define rest
                 (let loop ([a (cdr (syntax-e stx))] [head? #f])
                   (cond
                     [(identifier? a) (datum->syntax #f (syntax-e a) a a)]
                     [(and head? (pair? a) (and (identifier? (car a))
                                                (free-identifier=? #'unsyntax (car a))))
                      a]
                     [(pair? a) (cons (loop (car a) #t) 
                                      (loop (cdr a) #f))]
                     [(syntax? a) (datum->syntax a
                                                 (loop (syntax-e a) head?)
                                                 a 
                                                 a)]
                     [else a])))
               (datum->syntax stx (cons s rest) stx stx)]
              [else stx])))]))

(define-syntax racketmodname
  (syntax-rules (unsyntax)
    [(racketmodname #,n)
     (let ([sym n])
       (as-modname-link sym (to-element sym) #f))]
    [(racketmodname n)
     (as-modname-link 'n (**racketmodname n) #f)]
    [(racketmodname #,n #:indirect)
     (let ([sym n])
       (as-modname-link sym (to-element sym) #t))]
    [(racketmodname n #:indirect)
     (as-modname-link 'n (**racketmodname n) #t)]))

(define-syntax racketmodlink
  (syntax-rules (unsyntax)
    [(racketmodlink n content ...)
     (*as-modname-link 'n (elem #:style #f content ...) #f)]))

(define (as-modname-link s e indirect?)
  (if (symbol? s)
      (*as-modname-link s e indirect?)
      e))

(define-on-demand indirect-module-link-color
  (struct-copy style module-link-color
               [properties (cons 'indirect-link
                                 (style-properties module-link-color))]))

(define (*as-modname-link s e indirect?)
  (make-link-element (if indirect?
                         indirect-module-link-color
                         module-link-color)
                     (list e)
                     `(mod-path ,(datum-intern-literal (format "~s" s)))))

(define-syntax-rule (indexed-racket x)
  (add-racket-index 'x (racket x)))

(define (add-racket-index s e)
  (define k
    (cond [(and (pair? s) (eq? (car s) 'quote)) (format "~s" (cadr s))]
          [(string? s) s]
          [else (format "~s" s)]))
  (index* (list k) (list e) e))

(define-syntax-rule (define-/form id base)
  (define-syntax (id stx)
    (syntax-case stx ()
      [(_ a)
       ;; Remove the context from any ellipsis in `a`:
       (with-syntax ([a (strip-ellipsis-context #'a)])
         #'(base a))])))

(define-for-syntax (strip-ellipsis-context a)
  (define a-ellipsis (datum->syntax a '...))
  (define a-ellipsis+ (datum->syntax a '...+))
  (let loop ([a a])
    (cond
     [(identifier? a)
      (cond
        [(free-identifier=? a a-ellipsis #f)
         (datum->syntax #f '... a a)]
        [(free-identifier=? a a-ellipsis+ #f)
         (datum->syntax #f '...+ a a)]
        [else a])]
     [(syntax? a)
      (datum->syntax a (loop (syntax-e a)) a a)]
     [(pair? a)
      (cons (loop (car a))
            (loop (cdr a)))]
     [(vector? a)
      (list->vector
       (map loop (vector->list a)))]
     [(box? a)
      (box (loop (unbox a)))]
     [(prefab-struct-key a)
      => (lambda (k)
           (apply make-prefab-struct
                  k
                  (loop (cdr (vector->list (struct->vector a))))))]
     [else a])))

(define-/form racketblock0/form racketblock0)
(define-/form racketblock/form racketblock)
(define-/form racket/form racket)

(define (*racketlink stx-id id style . s)
  (define content (decode-content s))
  (make-delayed-element
   (lambda (r p ri)
     (make-link-element
      style
      content
      (or (find-racket-tag p ri stx-id #f)
          `(undef ,(format "--UNDEFINED:~a--" (syntax-e stx-id))))))
   (lambda () content)
   (lambda () content)))

(define-syntax racketlink
  (syntax-rules ()
    [(_ id #:style style . content)
     (*racketlink (quote-syntax id) 'id style . content)]
    [(_ id . content)
     (*racketlink (quote-syntax id) 'id #f . content)]))

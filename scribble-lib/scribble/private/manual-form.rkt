#lang racket/base
(require "../decode.rkt"
         "../struct.rkt"
         "../racket.rkt"
         (submod "../racket.rkt" id-element)
         "../basic.rkt"
         "../manual-struct.rkt"
         "qsloc.rkt"
         "manual-utils.rkt"
         "manual-vars.rkt"
         "manual-scheme.rkt"
         "manual-bind.rkt"
         racket/deprecation
         racket/list
         (for-syntax racket/base
                     syntax/parse
                     racket/syntax)
         (for-label racket/base))

(provide defform defform* defform/subs defform*/subs defform/none
         defidform defidform/inline
         specform specform/subs
         specsubform specsubform/subs specspecsubform specspecsubform/subs
         specsubform/inline
         defsubidform defsubform defsubform*
         racketgrammar racketgrammar*
         schemegrammar
         schemegrammar*
         var svar
         (for-syntax kind-kw id-kw link-target?-kw
                     literals-kw subs-kw contracts-kw))


(define-deprecated-alias schemegrammar racketgrammar)
(define-deprecated-alias schemegrammar* racketgrammar*)


(begin-for-syntax
 (define-splicing-syntax-class kind-kw
   #:description "#:kind keyword"
   (pattern (~seq #:kind kind))
   (pattern (~seq)
            #:with kind #'#f))

 (define-splicing-syntax-class id-kw
   #:description "#:id keyword"
   (pattern (~seq #:id [(defined-id:id ...) defined-id-expr]))
   (pattern (~seq #:id [one-defined-id:id defined-id-expr])
            #:with (defined-id ...) #'(one-defined-id))
   (pattern (~seq #:id one-defined-id:id)
            #:with defined-id-expr #'(quote-syntax one-defined-id)
            #:with (defined-id ...) #'(one-defined-id))
   (pattern (~seq #:id [#f #f])
            #:with (defined-id ...) #'(#f)
            #:with defined-id-expr #'#f)
   (pattern (~seq #:id [(#f) #f])
            #:with (defined-id ...) #'(#f)
            #:with defined-id-expr #'#f)
   (pattern (~seq)
            #:with (defined-id ...) #'(#f)
            #:with defined-id-expr #'#f))

 (define-splicing-syntax-class link-target?-kw
   #:description "#:link-target? keyword"
   (pattern (~seq #:link-target? expr))
   (pattern (~seq)
            #:with expr #'#t))

 (define-splicing-syntax-class literals-kw
   #:description "#:literals keyword"
   (pattern (~seq #:literals (lit:id ...)))
   (pattern (~seq)
            #:with (lit ...) #'()))

 (define-splicing-syntax-class contracts-kw
   #:description "#:contracts keyword"
   (pattern (~seq #:contracts (~and cs ([contract-nonterm:id contract-expr] ...))))
   (pattern (~seq)
            #:with (~and cs ((contract-nonterm contract-expr) ...)) #'()))

 (define-syntax-class grammar
   #:description "grammar"
   (pattern ([non-term-id:id non-term-form ...+] ...)))

 (define-splicing-syntax-class subs-kw
   #:description "#:grammar keyword"
   #:attributes (g (g.non-term-id 1) (g.non-term-form 2))
   (pattern (~seq #:grammar g:grammar))
   (pattern (~seq) #:with g:grammar #'()))
 )

(define-syntax (defform*/subs stx)
  (syntax-parse stx
    [(_ k:kind-kw lt:link-target?-kw d:id-kw l:literals-kw [spec spec1 ...]
        g:grammar
        c:contracts-kw
        desc:expr ...)
     (with-syntax* ([(defined-id ...) (if (ormap syntax-e (syntax->list #'(d.defined-id ...)))
                                          #'(d.defined-id ...)
                                          (syntax-case #'spec ()
                                            [(spec-id . _) #'(spec-id)]))]
                    [defined-id-expr (if (syntax-e #'d.defined-id-expr)
                                         #'d.defined-id-expr
                                         ;; there will be one `defined-id`
                                         #'(quote-syntax defined-id ...))]
                    [(x ...) (generate-temporaries #'(defined-id ...))])
       (define-values (add-unused is-used?s)
         (make-unused-name-adder #'spec (syntax->list #'(defined-id ...)) (syntax->list #'(x ...))))
       (with-syntax* ([(is-used? ...) is-used?s]
                      [(new-spec ...)
                       (let ([defined-ids (syntax->list #'(defined-id ...))]
                             [xs (syntax->list #'(x ...))])
                         (for/list ([spec (in-list (syntax->list #'(spec spec1 ...)))]
                                    [pos (in-naturals)])
                           (define add-unused-now (if (eqv? pos 0)
                                                      add-unused
                                                      (lambda (x) x)))
                           (let loop ([spec spec])
                             (or (and (identifier? spec)
                                      (for/or ([defined-id (in-list defined-ids)]
                                               [x (in-list xs)])
                                        (and (free-identifier=? spec defined-id)
                                             (datum->syntax #'here `(unsyntax ,(add-unused x)) spec spec))))
                                 (syntax-case spec ()
                                   [(a . b)
                                    (datum->syntax spec
                                                   (cons (loop #'a) (loop #'b))
                                                   spec
                                                   spec)]
                                   [_ spec])))))])
         #'(with-togetherable-racket-variables
             (l.lit ...)
             ([form [(defined-id ...) spec]] [form [(defined-id ...) spec1]] ...
                                             [non-term (g.non-term-id g.non-term-form ...)] ...)
             (*defforms k.kind lt.expr defined-id-expr
                        '(is-used? ...)
                        '(spec spec1 ...)
                        (list (lambda (x ...) (racketblock0/form new-spec)) ...)
                        '((g.non-term-id g.non-term-form ...) ...)
                        (list (list (lambda () (racket g.non-term-id))
                                    (lambda () (racketblock0/form g.non-term-form))
                                    ...)
                              ...)
                        (list (list (lambda () (racket c.contract-nonterm))
                                    (lambda () (racketblock0 c.contract-expr)))
                              ...)
                        (lambda () (list desc ...))))))]))

(define-syntax (defform* stx)
  (syntax-parse stx
    [(_ k:kind-kw lt:link-target?-kw d:id-kw l:literals-kw [spec ...+]
        subs:subs-kw c:contracts-kw desc:expr ...)
     (syntax/loc stx
       (defform*/subs #:kind k.kind 
         #:link-target? lt.expr
         #:id [(d.defined-id ...) d.defined-id-expr]
         #:literals (l.lit ...)
         [spec ...] subs.g #:contracts c.cs desc ...))]))

(define-syntax (defform stx)
  (syntax-parse stx
    [(_ k:kind-kw lt:link-target?-kw d:id-kw l:literals-kw spec
        subs:subs-kw c:contracts-kw desc:expr ...)
     (syntax/loc stx
       (defform*/subs #:kind k.kind
         #:link-target? lt.expr
         #:id [(d.defined-id ...) d.defined-id-expr]
         #:literals (l.lit ...)
         [spec] subs.g #:contracts c.cs desc ...))]))

(define-syntax (defform/subs stx)
  (syntax-parse stx
    [(_ k:kind-kw lt:link-target?-kw d:id-kw l:literals-kw spec subs c:contracts-kw desc:expr ...)
     (syntax/loc stx
       (defform*/subs #:kind k.kind 
         #:link-target? lt.expr
         #:id [(d.defined-id ...) d.defined-id-expr]
         #:literals (l.lit ...)
         [spec] subs #:contracts c.cs desc ...))]))

(define-syntax (defform/none stx)
  (syntax-parse stx
    [(_ k:kind-kw lt:link-target?-kw l:literals-kw spec subs:subs-kw c:contracts-kw desc:expr ...)
     (syntax/loc stx
       (with-togetherable-racket-variables
        (l.lit ...)
        ([form/none spec]
         [non-term (subs.g.non-term-id subs.g.non-term-form ...)] ...)
        (*defforms k.kind lt.expr #f
                   '(#t)
                   '(spec)
                   (list (lambda (ignored) (racketblock0/form spec)))
                   '((subs.g.non-term-id subs.g.non-term-form ...) ...)
                   (list (list (lambda () (racket subs.g.non-term-id))
                               (lambda () (racketblock0/form subs.g.non-term-form))
                               ...)
                         ...)
                   (list (list (lambda () (racket c.contract-nonterm))
                               (lambda () (racketblock0 c.contract-expr)))
                         ...)
                   (lambda () (list desc ...)))))]))

(define-syntax (defidform/inline stx)
  (syntax-case stx (unsyntax)
    [(_ id)
     (identifier? #'id)
     #'(defform-site (quote-syntax id))]
    [(_ (unsyntax id-expr))
     #'(defform-site id-expr)]))

(define-syntax (defidform stx)
  (syntax-parse stx
    [(_ k:kind-kw lt:link-target?-kw spec-id desc:expr ...)
     #'(with-togetherable-racket-variables
        ()
        ()
        (*defforms k.kind lt.expr (quote-syntax/loc spec-id)
                   '(#t)
                   '(spec-id)
                   (list (lambda (x) (make-omitable-paragraph (list x))))
                   null
                   null
                   null
                   (lambda () (list desc ...))))]))

(define (into-blockquote s)
  (make-blockquote "leftindent"
                   (if (splice? s)
                     (flow-paragraphs (decode-flow (splice-run s)))
                     (list s))))

(define-syntax (defsubidform stx)
  (syntax-case stx ()
    [(_ . rest) #'(into-blockquote (defidform . rest))]))

(define-syntax (defsubform stx)
  (syntax-case stx ()
    [(_ . rest) #'(into-blockquote (defform . rest))]))

(define-syntax (defsubform* stx)
  (syntax-case stx ()
    [(_ . rest) #'(into-blockquote (defform* . rest))]))

(define-syntax (spec?form/subs stx)
  (syntax-parse stx
    [(_ has-kw? l:literals-kw spec g:grammar
        c:contracts-kw
        desc:expr ...)
     (syntax/loc stx
       (with-racket-variables
        (l.lit ...)
        ([form/maybe (has-kw? spec)]
         [non-term (g.non-term-id g.non-term-form ...)] ...)
        (*specsubform 'spec '(l.lit ...) (lambda () (racketblock0/form spec))
                      '((g.non-term-id g.non-term-form ...) ...)
                      (list (list (lambda () (racket g.non-term-id))
                                  (lambda () (racketblock0/form g.non-term-form))
                                  ...)
                            ...)
                      (list (list (lambda () (racket c.contract-nonterm))
                                  (lambda () (racketblock0 c.contract-expr)))
                            ...)
                      (lambda () (list desc ...)))))]))

(define-syntax (specsubform stx)
  (syntax-parse stx
    [(_ l:literals-kw spec subs:subs-kw c:contracts-kw desc:expr ...)
     (syntax/loc stx
       (spec?form/subs #f #:literals (l.lit ...) spec subs.g #:contracts c.cs desc ...))]))

(define-syntax (specsubform/subs stx)
  (syntax-parse stx
    [(_ l:literals-kw spec g:grammar c:contracts-kw desc:expr ...)
     (syntax/loc stx
       (spec?form/subs #f #:literals (l.lit ...) spec 
                       ([g.non-term-id g.non-term-form ...] ...)
                       #:contracts c.cs
                       desc ...))]))

(define-syntax-rule (specspecsubform spec desc ...)
  (make-blockquote "leftindent" (list (specsubform spec desc ...))))

(define-syntax-rule (specspecsubform/subs spec subs desc ...)
  (make-blockquote "leftindent" (list (specsubform/subs spec subs desc ...))))

(define-syntax (specform stx)
  (syntax-parse stx
    [(_ l:literals-kw spec subs:subs-kw c:contracts-kw desc ...)
     (syntax/loc stx
       (spec?form/subs #t #:literals (l.lit ...) spec subs.g #:contracts c.cs desc ...))]))

(define-syntax (specform/subs stx)
  (syntax-parse stx
    [(_ l:literals-kw spec g:grammar c:contracts-kw
        desc:expr ...)
     (syntax/loc stx
       (spec?form/subs #t #:literals (l.lit ...) spec ([g.non-term-id g.non-term-form ...] ...)
                       #:contracts c.cs
                       desc ...))]))

(define-syntax-rule (specsubform/inline spec desc ...)
  (with-racket-variables
   ()
   ([form/maybe (#f spec)])
   (*specsubform 'spec null #f null null null (lambda () (list desc ...)))))

(define-syntax racketgrammar
  (syntax-rules ()
    [(_ #:literals (lit ...) id clause ...)
     (racketgrammar* #:literals (lit ...) [id clause ...])]
    [(_ id clause ...) (racketgrammar #:literals () id clause ...)]))

(define-syntax racketgrammar*
  (syntax-rules ()
    [(_ #:literals (lit ...) [id clause ...] ...)
     (with-racket-variables
      (lit ...)
      ([non-term (id clause ...)] ...)
      (*racketgrammar '(lit ...)
                      '(id ... clause ... ...)
                      (lambda ()
                        (list (list (racket id)
                                    (racketblock0/form clause) ...)
                              ...))))]
    [(_ [id clause ...] ...)
     (racketgrammar* #:literals () [id clause ...] ...)]))

(define-syntax-rule (var id)
  (*var 'id))

(define-syntax-rule (svar id)
  (*var 'id))


(define (meta-symbol? s) (memq s '(... ...+ ?)))

(define (defform-site kw-id #:kind [kind "syntax"] #:is-used? [is-used? #t])
  (define target-maker (id-to-form-target-maker kw-id #t))
  (define str (syntax-property kw-id 'display-string))
  (define-values (content ref-content) (definition-site (syntax-e kw-id) kw-id #t str))
  (if target-maker
      (target-maker
       content
       (lambda (tag)
         (make-toc-target2-element
          #f
          (if kw-id
              (make-index-element
               #f (if is-used? content null) tag
               (list (datum-intern-literal (symbol->string (syntax-e kw-id))))
               (list ref-content)
               (with-exporting-libraries
                   (lambda (libs)
                     (make-exported-index-desc* (syntax-e kw-id)
                                                libs
                                                (hash 'kind kind)))))
              content)
          tag
          ref-content)))
      (if is-used?
          content
          null)))

(define (*defforms kind link? kw-id/s is-used?s forms form-procs subs sub-procs contract-procs content-thunk)
  (define kind* (or kind "syntax"))
  (define (map-kw-id/s kw/ids is-used?s proc)
    (if (list? kw/ids)
        (map proc kw-id/s is-used?s)
        (list (proc kw-id/s #t))))
  (parameterize ([current-meta-list '(... ...+)])
    (make-box-splice
     (cons
      (make-blockquote
       vertical-inset-style
       (list
        (make-table
         boxed-style
         (append
          (for/list ([form (in-list forms)]
                     [form-proc (in-list form-procs)]
                     [i (in-naturals)])
            (list
             ((if (zero? i) (add-background-label kind*) values)
              (list
               (apply
                (or form-proc
                    (lambda (x)
                      (make-omitable-paragraph
                       (list (to-element `(,x . ,(cdr form)))))))
                (if kw-id/s
                    (map-kw-id/s
                     kw-id/s
                     is-used?s
                     (lambda (kw-id is-used?)
                       (define linking? (and link? (eq? form (car forms))))
                       (if linking?
                           (defform-site kw-id #:kind kind* #:is-used? is-used?)
                           (to-element #:defn? #t kw-id))))
                    (list #f)))))))
          (if (null? sub-procs)
              null
              (list (list flow-empty-line)
                    (list (make-flow
                           (list (let ([l (map (lambda (sub)
                                                 (map (lambda (f) (f)) sub))
                                               sub-procs)])
                                   (*racketrawgrammars "specgrammar"
                                                       (map car l)
                                                       (map cdr l))))))))
          (make-contracts-table contract-procs)))))
      (content-thunk)))))

(define (*specsubform form lits form-thunk subs sub-procs contract-procs content-thunk)
  (parameterize ([current-meta-list '(... ...+)])
    (make-blockquote
     "leftindent"
     (cons
      (make-blockquote
       vertical-inset-style
       (list
        (make-table
         boxed-style
         (cons
          (list
           (make-flow
            (list
             (if form-thunk
                 (form-thunk)
                 (make-omitable-paragraph (list (to-element form)))))))
          (append
           (if (null? sub-procs)
               null
               (list (list flow-empty-line)
                     (list (make-flow
                            (list (let ([l (map (lambda (sub)
                                                  (map (lambda (f) (f)) sub))
                                                sub-procs)])
                                    (*racketrawgrammars "specgrammar"
                                                        (map car l)
                                                        (map cdr l))))))))
           (make-contracts-table contract-procs))))))
      (flow-paragraphs (decode-flow (content-thunk)))))))

(define (*racketrawgrammars style nonterms clauseses)
  (make-table
   `((valignment baseline baseline baseline baseline baseline)
     (alignment right left center left left)
     (style ,style))
   (cdr
    (append-map
     (lambda (nonterm clauses)
       (list*
        (list flow-empty-line flow-empty-line flow-empty-line
              flow-empty-line flow-empty-line)
        (list (to-flow nonterm) flow-empty-line (to-flow "=") flow-empty-line
              (make-flow (list (car clauses))))
        (for/list ([clause (in-list (cdr clauses))])
          (list flow-empty-line
                flow-empty-line
                (to-flow "|")
                flow-empty-line
                (make-flow (list clause))))))
     nonterms clauseses))))

(define (*racketrawgrammar style nonterm clause1 . clauses)
  (*racketrawgrammars style (list nonterm) (list (cons clause1 clauses))))

(define (*racketgrammar lits s-expr clauseses-thunk)
  (define l (clauseses-thunk))
  (*racketrawgrammars #f
                      (for/list ([x (in-list l)])
                        (make-element #f (list (hspace 2) (car x))))
                      (map cdr l)))

(define (*var id)
  (to-element (*var-sym id)))

(define (*var-sym id)
  (string->symbol (format "_~a" id)))

(define (make-contracts-table contract-procs)
  (if (null? contract-procs)
      null
      (append
       (list (list flow-empty-line))
       (list (list (make-flow
                    (for/list ([c (in-list contract-procs)])
                      (make-table "argcontract"
                                  (list (list (to-flow (hspace 2))
                                              (to-flow ((car c)))
                                              flow-spacer
                                              (to-flow ":")
                                              flow-spacer
                                              (make-flow (list ((cadr c))))))))))))))

(define-for-syntax (make-unused-name-adder spec defined-ids xs)
  (define unused xs)
  (let loop ([spec spec])
    (cond
      [(null? unused) (void)]
      [(identifier? spec)
       (for ([defined-id (in-list defined-ids)]
             [x (in-list xs)])
         (when (free-identifier=? spec defined-id)
           (set! unused (remq x unused))))]
      [(syntax? spec)
       (loop (syntax-e spec))]
      [(pair? spec)
       (loop (car spec))
       (loop (cdr spec))]))
  (values
   (lambda (x)
     (if (null? unused)
         x
         (let ([e `(make-element #f (list ,x ,@unused))])
           (set! unused null)
           e)))
   (for/list ([x (in-list xs)])
     (not (memq x unused)))))

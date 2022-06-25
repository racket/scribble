#lang racket/base
(require racket/string
         racket/format
         "../struct.rkt"
         "../scheme.rkt"
         "../search.rkt"
         "../basic.rkt"
         "../manual-struct.rkt"
         (only-in "../core.rkt" make-style)
         "../html-properties.rkt"
         "manual-ex.rkt"
         racket/contract/base
         (for-syntax racket/base)
         (for-label racket/base
                    racket/class))

(provide definition-site
         libs->taglet
         annote-exporting-library
         with-exporting-libraries
         id-to-target-maker
         id-to-form-target-maker
         *sig-elem
         (struct-out sig)
         ;; public:
         ; XXX unknown contract
         make-binding-redirect-elements
         sigelem)
(provide/contract
 ; XXX What is return type?
 [defidentifier ((identifier?) (#:form? boolean? #:index? boolean? #:show-libs? boolean?) . ->* . any/c)])

(define (gen-absolute-tag)
  `(abs ,(make-generated-tag)))

(define-struct sig (id))

(define-syntax-rule (sigelem sig elem)
  (*sig-elem (quote-syntax sig) 'elem))

(define (*sig-elem sig elem #:defn? [defn? #f])
  (let ([s (to-element/no-color elem)])
    (make-delayed-element
     (lambda (renderer sec ri)
       (let* ([tag (find-scheme-tag sec ri sig #f)]
              [taglet (and tag (append (cadr tag) (list elem)))]
              [vtag (and tag `(sig-val ,taglet))]
              [stag (and tag `(sig-form ,taglet))]
              [sd (and stag (resolve-get/tentative sec ri stag))])
         (make-element
          symbol-color
          (list
           (cond [sd (make-link-element (if defn? syntax-def-color syntax-link-color) (list s) stag)]
                 [vtag (make-link-element (if defn? value-def-color value-link-color) (list s) vtag)]
                 [else s])))))
     (lambda () s)
     (lambda () s))))

(define hovers (make-weak-hasheq))
(define (intern-hover-style text)
  (let ([text (datum-intern-literal text)])
    (or (hash-ref hovers text #f)
        (let ([s (make-style #f (list (make-hover-property text)))])
          (hash-set! hovers text s)
          s))))

(define (annote-exporting-library e)
  (make-delayed-element
   (lambda (render p ri)
     (let ([from (resolve-get/tentative p ri '(exporting-libraries #f))])
       (if (and from (pair? from))
           (make-element
            (intern-hover-style
             (string-append
              "Provided from: "
              (string-join (map ~s from) ", ")
              (let ([from-pkgs (resolve-get/tentative p ri '(exporting-packages #f))])
                (if (and from-pkgs (pair? from-pkgs))
                    (string-append
                     " | Package: "
                     (string-join (map ~a from-pkgs) ", "))
                    ""))))
            e)
           e)))
   (lambda () e)
   (lambda () e)))

(define (get-exporting-libraries render p ri)
  (resolve-get/tentative p ri '(exporting-libraries #f)))

(define (with-exporting-libraries proc)
  (make-delayed-index-desc
   (lambda (render part ri)
     (proc (or (get-exporting-libraries render part ri) null)))))

(define (definition-site name stx-id form?)
  (let ([sig (current-signature)])
    (define (gen defn?)
      (if sig
          (*sig-elem #:defn? defn? (sig-id sig) name)
          ((if defn? annote-exporting-library values)
           (to-element #:defn? defn? (make-just-context name stx-id)))))
    (values (gen #t) (gen #f))))

(define checkers (make-hash))

(define (libs->taglet id libs source-libs)
  (let ([lib
         (or (ormap (lambda (lib)
                      (let ([checker
                             (hash-ref
                              checkers lib
                              (lambda ()
                                (let ([ns-id 
                                       (let ([ns (make-base-empty-namespace)])
                                         (parameterize ([current-namespace ns])
                                           ;; A `(namespace-require `(for-label ,lib))` can
                                           ;; fail if `lib` provides different bindings of the
                                           ;; same name at different phases. We can require phases
                                           ;; 1 and 0 separately, in which case the phase-0
                                           ;; binding shadows the phase-1 one in that case.
                                           ;; This strategy only works for documenting bindings
                                           ;; at phases 0 and 1, though.
                                           (namespace-require `(just-meta 1 (for-label ,lib)))
                                           (namespace-require `(just-meta 0 (for-label ,lib)))
                                           (namespace-syntax-introduce (datum->syntax #f 'x))))])
                                  (let ([checker
                                         (lambda (id)
                                           (free-label-identifier=?
                                            (datum->syntax ns-id (syntax-e id))
                                            id))])
                                    (hash-set! checkers lib checker)
                                    checker))))])
                        (and (checker id) lib)))
                    (or source-libs null))
             (and (pair? libs) (car libs)))])
    (and lib (module-path-index->taglet
              (module-path-index-join lib #f)))))

(define (id-to-target-maker id dep? #:space [space #f])
  (*id-to-target-maker 'def id dep? #:space space))

(define (id-to-form-target-maker id dep? #:space [space #f])
  (*id-to-target-maker 'form id dep? #:space space))

(define (*id-to-target-maker sym id dep? #:space [space #f])
  (let ([sig (current-signature)])
    (lambda (content mk)
      (make-part-relative-element
       (lambda (ci)
         (let ([e (ormap (lambda (p)
                           (ormap (lambda (e)
                                    (and (exporting-libraries? e) e))
                                  (part-to-collect p)))
                         (collect-info-parents ci))])
           (unless e
             ;; Call raise-syntax-error to capture error message:
             (with-handlers ([exn:fail:syntax?
                              (lambda (exn)
                                (eprintf "~a\n" (exn-message exn)))])
               (raise-syntax-error
                'WARNING
                "no declared exporting libraries for definition" id)))
           (if e
             (let* ([lib-taglet (libs->taglet
                                 (if sig (sig-id sig) id)
                                 (exporting-libraries-libs e)
                                 (exporting-libraries-source-libs e))]
                    [tag (intern-taglet
                          (list (if sig
                                  (case sym
                                    [(def) 'sig-val]
                                    [(form) 'sig-def])
                                  sym)
                                `(,lib-taglet
                                  ,@(if sig (list (syntax-e (sig-id sig))) null)
                                  ,(syntax-e id)
                                  ,@(if space (list space) null))))])
               (if (or sig (not dep?))
                   (mk tag)
                   (make-dep (list* lib-taglet (syntax-e id) (if space (list space) null))
                             (mk tag))))
             content)))
       (lambda () content)
       (lambda () content)))))

(define (defidentifier id 
                       #:form? [form? #f]
                       #:index? [index? #t]
                       #:show-libs? [show-libs? #t])
  ;; This function could have more optional argument to select
  ;; whether to index the id, include a toc link, etc.
  (let ([dep? #t])
    (let ([maker (if form?
                     (id-to-form-target-maker id dep?)
                     (id-to-target-maker id dep?))])
      (define-values (elem elem-ref)
        (if show-libs?
            (definition-site (syntax-e id) id form?)
            (values (to-element id #:defn? #t)
                    (to-element id))))
      (if maker
          (maker elem
                 (lambda (tag)
                   (let ([elem
                          (if index?
                              (make-index-element
                               #f (list elem) tag
                               (list (datum-intern-literal (symbol->string (syntax-e id))))
                               (list elem)
                               (and show-libs?
                                    (with-exporting-libraries
                                     (lambda (libs)
                                       (make-exported-index-desc (syntax-e id)
                                                                 libs)))))
                              elem)])
                     (make-target-element #f (list elem) tag))))
          elem))))

(define (make-binding-redirect-elements mod-path redirects)
  (let ([taglet (module-path-index->taglet 
                 (module-path-index-join mod-path #f))])
    (make-element
     #f
     (map
      (lambda (redirect)
        (let ([id (car redirect)]
              [form? (cadr redirect)]
              [path (caddr redirect)]
              [anchor (cadddr redirect)])
          (let ([make-one
                 (lambda (kind)
                   (make-redirect-target-element
                    #f
                    null
                    (intern-taglet (list kind (list taglet id)))
                    path
                    anchor))])
            (make-element
             #f
             (list (make-one (if form? 'form 'def))
                   (make-dep (list taglet id) null)
                   (let ([str (datum-intern-literal (symbol->string id))])
                     (make-index-element #f
                                         null
                                         (intern-taglet
                                          (list (if form? 'form 'def)
                                                (list taglet id)))
                                         (list str)
                                         (list
                                          (make-element
                                           symbol-color
                                           (list
                                            (make-element
                                             (if form?
                                                 syntax-link-color
                                                 value-link-color)
                                             (list str)))))
                                         (make-exported-index-desc*
                                          id
                                          (list mod-path)
                                          (list (if form?
                                                    "syntax"
                                                    "procedure"))))))))))
      redirects))))


(define (make-dep t content)
  (make-collect-element
   #f
   content
   (lambda (ci)
     (collect-put! ci 
                   (intern-taglet (list 'dep t))
                   #t))))

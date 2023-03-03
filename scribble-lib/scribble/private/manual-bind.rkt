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
  (define s (to-element/no-color elem))
  (make-delayed-element
   (lambda (renderer sec ri)
     (define tag (find-scheme-tag sec ri sig #f))
     (define taglet (and tag (append (cadr tag) (list elem))))
     (define vtag (and tag `(sig-val ,taglet)))
     (define stag (and tag `(sig-form ,taglet)))
     (define sd (and stag (resolve-get/tentative sec ri stag)))
     (make-element
      symbol-color
      (list
       (cond [sd (make-link-element (if defn? syntax-def-color syntax-link-color) (list s) stag)]
             [vtag (make-link-element (if defn? value-def-color value-link-color) (list s) vtag)]
             [else s]))))
   (lambda () s)
   (lambda () s)))

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
     (define from (resolve-get/tentative p ri '(exporting-libraries #f)))
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
         e))
   (lambda () e)
   (lambda () e)))

(define (get-exporting-libraries render p ri)
  (resolve-get/tentative p ri '(exporting-libraries #f)))

(define (with-exporting-libraries proc)
  (make-delayed-index-desc
   (lambda (render part ri)
     (proc (or (get-exporting-libraries render part ri) null)))))

(define (definition-site name stx-id form?)
  (define sig (current-signature))
  (define (gen defn?)
    (if sig
        (*sig-elem #:defn? defn? (sig-id sig) name)
        ((if defn? annote-exporting-library values)
         (to-element #:defn? defn? (make-just-context name stx-id)))))
  (values (gen #t) (gen #f)))

(define checkers (make-hash))

(define (libs->taglet id libs source-libs [space #f])
  (unless (or (not space)
              (symbol? space))
    (raise-argument-error 'libs->taglet "(or/c #f symbol?)" space))
  (define intro
    (if space
        (make-interned-syntax-introducer space)
        (lambda (x add) x)))
  (let ([lib
         (or (for/or ([lib (in-list (or source-libs null))])
               (let ([checker
                      (hash-ref
                       checkers lib
                       (lambda ()
                         (define ns-id 
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
                               (namespace-syntax-introduce (datum->syntax #f 'x)))))
                         (define (checker id intro)
                           (free-label-identifier=?
                            (intro (datum->syntax ns-id (syntax-e id)) 'add)
                            (intro id 'add)))
                         (hash-set! checkers lib checker)
                         checker))])
                 (and (checker id intro) lib)))
             (and (pair? libs) (car libs)))])
    (and lib (module-path-index->taglet
              (module-path-index-join lib #f)))))

(define (id-to-target-maker id dep? #:space [space #f] #:suffix [suffix space])
  (*id-to-target-maker 'def id dep? #:space space #:suffix suffix))

(define (id-to-form-target-maker id dep? #:space [space #f] #:suffix [suffix space])
  (*id-to-target-maker 'form id dep? #:space space #:suffix suffix))

(define (*id-to-target-maker sym id dep? #:space space #:suffix suffix)
  (define sig (current-signature))
  (lambda (content mk)
    (make-part-relative-element
     (lambda (ci)
       (let ([e (for/or ([p (in-list (collect-info-parents ci))])
                  (ormap (lambda (e)
                           (and (exporting-libraries? e) e))
                         (part-to-collect p)))])
         (unless e
           ;; Call raise-syntax-error to capture error message:
           (with-handlers ([exn:fail:syntax?
                            (lambda (exn)
                              (eprintf "~a\n" (exn-message exn)))])
             (raise-syntax-error
              'WARNING
              "no declared exporting libraries for definition" id)))
         (cond
           [e
            (define lib-taglet
              (libs->taglet
               (if sig (sig-id sig) id)
               (exporting-libraries-libs e)
               (exporting-libraries-source-libs e)
               space))
            (define tag
              (intern-taglet
               (list (if sig
                         (case sym
                           [(def) 'sig-val]
                           [(form) 'sig-def])
                         sym)
                     `(,lib-taglet
                       ,@(if sig (list (syntax-e (sig-id sig))) null)
                       ,(syntax-e id)
                       ,@(if suffix (list suffix) null)))))
            (if (or sig (not dep?))
                (mk tag)
                (make-dep (list* lib-taglet (syntax-e id) (if suffix (list suffix) null))
                          (mk tag)))]
           [else content])))
     (lambda () content)
     (lambda () content))))

(define (defidentifier id 
                       #:form? [form? #f]
                       #:index? [index? #t]
                       #:show-libs? [show-libs? #t])
  ;; This function could have more optional argument to select
  ;; whether to index the id, include a toc link, etc.
  (let ([dep? #t])
    (define maker
      (if form?
          (id-to-form-target-maker id dep?)
          (id-to-target-maker id dep?)))
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
        elem)))

(define (make-binding-redirect-elements mod-path redirects)
  (define taglet (module-path-index->taglet 
                  (module-path-index-join mod-path #f)))
  (make-element
   #f
   (map
    (lambda (redirect)
      (define id (car redirect))
      (define form? (cadr redirect))
      (define path (caddr redirect))
      (define anchor (cadddr redirect))
      (define (make-one kind)
        (make-redirect-target-element
         #f
         null
         (intern-taglet (list kind (list taglet id)))
         path
         anchor))
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
                                   ((if form?
                                        make-form-index-desc
                                        make-procedure-index-desc)
                                    id
                                    (list mod-path)))))))
    redirects)))


(define (make-dep t content)
  (make-collect-element
   #f
   content
   (lambda (ci)
     (collect-put! ci 
                   (intern-taglet (list 'dep t))
                   #t))))

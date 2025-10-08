#lang racket/base

(require scribble/struct
         (only-in scribble/core known-doc? known-doc-v)
         scribble/base-render
         scribble/search
         (prefix-in html: scribble/html-render)
         racket/class
         racket/path)

(provide load-xref
         xref?
         xref-render
         xref-index
         xref-binding->definition-tag
         xref-tag->path+anchor
         xref-tag->index-entry
         xref-transfer-info
         (struct-out entry)
         make-data+root
         data+root?
         make-data+root+doc-id
         data+root+doc-id?
         make-data+root+doc-id+pkg
         data+root+doc-id+pkg?)

(define-struct entry
  (words    ; list of strings: main term, sub-term, etc.
   content  ; Scribble content to the index label
   tag      ; for generating a Scribble link
   desc))   ; further info that depends on the kind of index entry

(define-struct data+root (data root))
(define-struct (data+root+doc-id data+root) (doc-id))
(define-struct (data+root+doc-id+pkg data+root+doc-id) (pkg))

;; Private:
(define-struct xrefs (renderer ri))

(define (xref? x) (xrefs? x))

;; ----------------------------------------
;; Xref loading

(define-namespace-anchor here)

(define (load-xref sources
                   #:demand-source [demand-source (lambda (key) #f)]
                   #:demand-source-for-use [demand-source-for-use
                                            (lambda (key use-id) (demand-source key))]
                   #:render% [render% (html:render-mixin render%)]
                   #:root [root-path #f]
                   #:doc-id [doc-id-str #f]
                   #:pkg [pkg-str #f])
  (let* ([renderer (new render% [dest-dir (find-system-path 'temp-dir)])]
         [fp (send renderer traverse null null)]
         [load-source (lambda (src ci)
                        (parameterize ([current-namespace (namespace-anchor->empty-namespace here)])
                          (define vs (src))
                          (for ([v (in-list (if (procedure? vs)
                                                (vs)
                                                (list vs)))])
                            (when v
                              (define data
                                (if (data+root? v)
                                    (data+root-data v)
                                    v))
                              (define root
                                (if (data+root? v)
                                    (data+root-root v)
                                    root-path))
                              (define doc-id
                                (or (and (data+root+doc-id? v) (data+root+doc-id-doc-id v))
                                    doc-id-str))
                              (define pkg
                                (or (and (data+root+doc-id+pkg? v) (data+root+doc-id+pkg-pkg v))
                                    pkg-str))
                              (send renderer deserialize-info
                                    data
                                    ci
                                    #:root root
                                    #:doc-id doc-id
                                    #:pkg pkg)))))]
         [use-ids (make-weak-hasheq)]
         [ci (send renderer collect null null fp
                   (lambda (key ci)
                     (define use-obj (collect-info-ext-ht ci))
                     (define use-id (hash-ref! use-ids use-obj (λ () (gensym 'render))))
                     (define src (demand-source-for-use key use-id))
                     (and src
                          (load-source src ci))))])
    (for ([src sources])
      (load-source src ci))
    (make-xrefs renderer (send renderer resolve null null ci))))

;; ----------------------------------------
;; Xref reading

(define (xref-index xrefs)
  (define ci (resolve-info-ci (xrefs-ri xrefs)))
  ;; Force all xref info:
  ((collect-info-ext-demand ci) #f ci)
  ;; look for `index-entry' keys:
  (for/list ([(k v) (in-hash (collect-info-ext-ht ci))]
             #:when
             (and (pair? k)
                  (eq? (car k) 'index-entry)))
    (let ([v (if (known-doc? v)
                 (known-doc-v v)
                 v)])
      (make-entry (car v) (cadr v) (cadr k) (caddr v)))))

;; dest-file can be #f, which will make it return a string holding the
;; resulting html
(define (xref-render xrefs doc dest-file
                     #:render% [render% (html:render-mixin render%)]
                     #:refer-to-existing-files? [use-existing? (not dest-file)])
  (let* ([dest-file (if (string? dest-file) (string->path dest-file) dest-file)]
         [renderer (new render%
                        [dest-dir (and dest-file (path-only dest-file))]
                        [refer-to-existing-files use-existing?]
                        [css-path    'inline]
                        [script-path 'inline])]
         [ci (send renderer collect (list doc) (list dest-file))]
         [_ (send renderer transfer-info ci (resolve-info-ci (xrefs-ri xrefs)))]
         [ri (send renderer resolve (list doc) (list dest-file) ci)]
         [xs (send renderer render (list doc) (list dest-file) ri)])
    (if dest-file
        (void)
        (car xs))))

(define (xref-transfer-info renderer ci xrefs)
  (send renderer transfer-info ci (resolve-info-ci (xrefs-ri xrefs))))

;; Returns (values <tag-or-#f> <form?>)
(define (xref-binding-tag xrefs id/binding mode #:space [space #f] #:suffix [suffix space])
  (define (search id/binding)
    (let ([tag (find-scheme-tag #f (xrefs-ri xrefs) id/binding mode #:space space #:suffix suffix)])
      (if tag
          (values tag (eq? (car tag) 'form))
          (values #f #f))))
  (cond
    [(identifier? id/binding) (search id/binding)]
    [(and (list? id/binding) (= 7 (length id/binding))) (search id/binding)]
    [(and (list? id/binding) (= 2 (length id/binding)))
     (let loop ([src (car id/binding)])
       (cond
         [(module-path-index? src) (search (list src (cadr id/binding)))]
         [(module-path? src) (loop (module-path-index-join src #f))]
         [else
          (raise-argument-error 'xref-binding-definition->tag
                                "(list/c (or/c module-path? module-path-index?) any/c)"
                                id/binding)]))]
    [else
     (raise-argument-error 'xref-binding-definition->tag
                           (string-append "(or/c identifier? (lambda (l)\n"
                                          "                    (and (list? l)\n"
                                          "                         (or (= (length l) 2)\n"
                                          "                             (= (length l) 7)))))")
                           id/binding)]))

(define (xref-binding->definition-tag xrefs
                                      id/binding
                                      mode
                                      #:space [space #f]
                                      #:suffix [suffix space])
  (define-values (tag form?) (xref-binding-tag xrefs id/binding mode #:space space #:suffix suffix))
  tag)

(define (xref-tag->path+anchor xrefs tag
                               #:render% [render% (html:render-mixin render%)]
                               #:external-root-url [redirect-main #f])
  (send (let ([r (new render% [dest-dir (find-system-path 'temp-dir)])])
          (when redirect-main
            (send r set-external-root-url redirect-main))
          r)
        tag->path+anchor (xrefs-ri xrefs) tag))

(define (xref-tag->index-entry xrefs tag)
  (define v
    (hash-ref (collect-info-ext-ht (resolve-info-ci (xrefs-ri xrefs))) `(index-entry ,tag) #f))
  (let ([v (if (known-doc? v)
               (known-doc-v v)
               v)])
    (cond
      [v (make-entry (car v) (cadr v) (cadr tag) (caddr v))]
      ;; Try again with 'def:
      [(and (pair? tag) (eq? 'form (car tag))) (xref-tag->index-entry xrefs (cons 'def (cdr tag)))]
      [else #f])))

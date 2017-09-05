#lang scheme/base
(require "../decode.rkt"
         "../struct.rkt"
         "../basic.rkt"
         "../manual-struct.rkt"
         (only-in "../core.rkt" table-columns)
         "manual-ex.rkt"
         "manual-style.rkt"
         "manual-scheme.rkt"
         "manual-utils.rkt"
         setup/main-collects
         pkg/path
         racket/list
         scribble/html-properties
         (for-syntax scheme/base
                     syntax/parse)
         (for-label scheme/base))

(provide defmodule defmodule* 
         defmodulelang defmodulelang* 
         defmodulereader defmodulereader*
         defmodule*/no-declare defmodulelang*/no-declare defmodulereader*/no-declare
         declare-exporting)

;; ---------------------------------------------------------------------------------------------------
(provide deprecated)

(require (only-in scribble/core make-style make-background-color-property)
         (only-in scribble/base para nested))

;; @deprecated[Precontent]{Precontent ... }
;; produces a nested paragraph with a yellow NOTE label to warn readers of deprecated modules 
(define (deprecated #:what [what "library"]
                    replacement
                    . additional-notes)
  (apply nested #:style 'inset
         (yellow (bold "NOTE:"))
         " This " what
         " is deprecated; use "
         replacement
         ", instead. "
         additional-notes))

(define (yellow . content)
  (make-element (make-style #f (list (make-background-color-property "yellow"))) content))
;; ---------------------------------------------------------------------------------------------------

(define-syntax (defmodule stx)
  (syntax-parse stx
    [(_ (~or (~seq #:require-form req)
             (~seq))
        (~or (~seq #:multi (name2 ...))
             name)
        (~or (~optional (~seq #:link-target? link-target-expr)
                        #:defaults ([link-target-expr #'#t]))
             (~optional (~and #:indirect indirect))
             (~optional (~seq #:use-sources (pname ...)))
             (~optional (~seq #:module-paths (modpath ...)))
             (~optional (~seq #:packages (pkg ...)))
             (~optional (~and #:no-declare no-declare))
             (~optional (~or (~and #:lang language)
                             (~and #:reader readr))))
        ...
        . content)
     (with-syntax ([(name2 ...) (if (attribute name)
                                    #'(name)
                                    #'(name2 ...))]
                   [(pname ...) (if (attribute pname)
                                    #'(pname ...)
                                    #'())]
                   [(indirect-kw ...) (if (attribute indirect)
                                          #'(#:indirect)
                                          #'())])
       (with-syntax ([(decl-exp ...)
                      (if (attribute no-declare)
                          #'()
                          (with-syntax ([(mod ...)
                                         (if (attribute modpath)
                                             #'(modpath ...)
                                             #'(name2 ...))]
                                        [(pkg-decl ...)
                                         (if (attribute pkg)
                                             #'(#:packages (pkg ...))
                                             #'())])
                            #'((declare-exporting mod ... pkg-decl ... #:use-sources (pname ...)))))]
                     [kind (cond
                            [(attribute language) #'#t]
                            [(attribute readr) #''reader]
                            [else #'#f])]
                     [modpaths (if (attribute modpath)
                                   #'(list (racketmodname modpath indirect-kw ...) ...)
                                   #'#f)]
                     [packages (if (attribute pkg)
                                   #'(list pkg ...)
                                   #'#f)]
                     [module-path (let ([l (syntax->list 
                                            (if (attribute modpath)
                                                #'(modpath ...)
                                                #'(name2 ...)))])
                                    (and (pair? l)
                                         (car l)))]
                     [req (if (attribute req)
                              #'req
                              #'(racket require))]
                     [(show-name ...)
                      (if (attribute modpath)
                          #'(name2 ...)
                          #'((racketmodname name2 indirect-kw ...) ...))])
         #'(begin
             decl-exp ...
             (*defmodule (list show-name ...)
                         modpaths
                         'module-path
                         packages
                         link-target-expr
                         kind
                         (list . content)
                         req))))]))

;; ----------------------------------------
;; old forms for backward compatibility:

(define-syntax defmodule*/no-declare
  (syntax-rules ()
    [(_ #:require-form req (name ...) . content)
     (defmodule #:require-form req
       #:names (name ...)
       #:no-declare
       . content)]
    [(_ (name ...) . content)
     (defmodule #:multi (name ...)
       #:no-declare
       . content)]))

(define-syntax defmodule*
  (syntax-rules ()
    [(_ #:require-form req (name ...) . options+content)
     (defmodule #:require-form req #:multi (name ...)
       . options+content)]
    [(_ (name ...) . options+content)
     (defmodule #:multi (name ...) . options+content)]))

(define-syntax defmodulelang*/no-declare
  (syntax-rules ()
    [(_ (lang ...) . options+content)
     (defmodule #:multi (lang ...)
       #:lang
       #:no-declare
       . options+content)]))

(define-syntax defmodulelang*
  (syntax-rules ()
    [(_ (name ...) . options+content)
     (defmodule #:multi (name ...)
       #:lang
       . options+content)]))

(define-syntax defmodulelang
  (syntax-rules ()
    [(_ lang #:module-path modpath . options+content)
     (defmodule lang
       #:module-paths (modpath)
       #:lang
       . options+content)]
    [(_ lang . options+content)
     (defmodule lang
       #:lang
       . options+content)]))

(define-syntax-rule (defmodulereader*/no-declare (lang ...) . options+content)
  (defmodule #:multi (lang ...)
    #:reader
    #:no-declare
    . options+content))

(define-syntax defmodulereader*
  (syntax-rules ()
    [(_ (name ...) . options+content)
     (defmodule #:multi (name ...)
       #:reader
       . options+content)]))

(define-syntax-rule (defmodulereader lang . options+content)
  (defmodule lang
    #:reader
    . options+content))

;; ----------------------------------------

(define (compute-packages module-path)
  (let* ([path (with-handlers ([exn:missing-module? (lambda (exn) #f)])
                 (and module-path
                      (resolved-module-path-name
                       (module-path-index-resolve (module-path-index-join module-path #f)))))]
         [pkg (and path
                   (path? path)
                   (or (path->pkg path)
                       (let ([c (path->main-collects-relative path)])
                         (and c
                              "base"))))])
    (if pkg
        (list pkg)
        null)))

;; mflatt thinks this should not be exposed
(define (racketpkgname pkg)
  (link
   ;; XXX Look at (pkg-info-orig-pkg (hash-ref (read-pkgs-db scope)
   ;; pkg)) and only show link if catalog? Or did mflatt have
   ;; something else in mind? But I'd have to know the scope and pass
   ;; that down from compute-packages
   (format "https://pkgs.racket-lang.org/package/~a" pkg)
   (tt pkg)
   #:style (make-style #f
                       (list "plainlink"
                             (hover-property
                              (format "Install this package using `raco pkg install ~a`"
                                      pkg))))))

(define (*defmodule names modpaths module-path packages link-target? lang content req)
  (let ([modpaths (or modpaths names)])
    (define pkg-spec
      (let ([pkgs (or packages
                      (compute-packages module-path))])
        (and pkgs
             (pair? pkgs)
             (make-flow
              (list
               (make-omitable-paragraph
                (list (elem #:style "RpackageSpec"
                            (list* (smaller 'nbsp
                                            (format "package~a:" 
                                                    (if (null? (cdr pkgs))
                                                        ""
                                                        "s")))
                                   " "
                                   (add-between (map racketpkgname pkgs)
                                                ", "))))))))))
    (define (flow-width f) (apply max (map block-width f)))
    (define libs-specs
      ;; make-desc  : element -> flow
      ;; index-desc : module-path-index-desc
      (let-values ([(make-desc index-desc)
                    (case lang
                      [(#f)
                       (values (lambda (modname) (list (racket (#,req #,modname))))
                               the-module-path-index-desc)]
                      [(#t)
                       (values (lambda (modname) (list (hash-lang) spacer modname))
                               the-language-index-desc)]
                      [(reader)
                       (values (lambda (modname) (list (racketmetafont "#reader") spacer modname))
                               the-reader-index-desc)]
                      [(just-lang)
                       (values (lambda (modname) (list (hash-lang) spacer modname))
                               the-language-index-desc)]
                      [else (error 'defmodule "unknown mode: ~e" lang)])])
      (map
       (lambda (name modpath)
         (define modname (if link-target?
                             (make-defracketmodname name modpath index-desc)
                             name))
         (list
          (make-flow
           (list
            (make-omitable-paragraph
             (cons spacer (make-desc modname)))))
          'cont))
       names
       modpaths)))

    (make-splice
     (cons
      (make-table
       (make-style "defmodule"
                   (list (table-columns (list
                                         (make-style #f '(left))
                                         (make-style #f '(right))))))
       (if pkg-spec
           (if ((+ (flow-width (caar libs-specs))
                   (flow-width pkg-spec)
                   8)
                . < . (current-display-width))
               (cons
                (cons (car (car libs-specs))
                      (list pkg-spec))
                (cdr libs-specs))
               (append
                libs-specs
                (list (list (make-flow (list (make-omitable-paragraph (list 'nbsp))))
                            pkg-spec))))
           libs-specs))
      (append (if link-target?
                  (map (lambda (modpath)
                         (make-part-tag-decl 
                          (intern-taglet
                           `(mod-path ,(datum-intern-literal
                                        (element->string modpath))))))
                       modpaths)
                  null)
              (flow-paragraphs (decode-flow content)))))))

(define the-module-path-index-desc (make-module-path-index-desc))
(define the-language-index-desc (make-language-index-desc))
(define the-reader-index-desc (make-reader-index-desc))

(define (make-defracketmodname mn mp index-desc)
  (let ([name-str (datum-intern-literal (element->string mn))]
        [path-str (datum-intern-literal (element->string mp))])
    (make-index-element #f
                        (list mn)
                        (intern-taglet `(mod-path ,path-str))
                        (list name-str)
                        (list mn)
                        index-desc)))

(define-syntax (declare-exporting stx)
  (syntax-parse stx
    [(_ lib:expr ... 
        (~optional (~seq #:packages (pkg ...)))
        (~optional (~seq #:use-sources (plib ...))))
     (with-syntax ([(plib ...) (if (attribute plib)
                                   #'(plib ...)
                                   #'())]
                   [packages (if (attribute pkg)
                                 #'(list pkg ...)
                                 #'#f)])
       (let ([libs (syntax->list #'(lib ... plib ...))])
         (for ([l libs])
           (unless (or (syntax-case l (unquote)
                         [(unquote _) #t]
                         [_ #f])
                       (module-path? (syntax->datum l)))
             (raise-syntax-error #f "not a module path" stx l)))
         (when (null? libs)
           (raise-syntax-error #f "need at least one module path" stx))
         #'(*declare-exporting `(lib ...) `(plib ...) packages)))]))

(define (*declare-exporting libs source-libs in-pkgs)
  (define pkgs (or in-pkgs
                   (if (null? libs)
                       null
                       (compute-packages (car libs)))))
  (make-splice
   (list
    (make-part-collect-decl
     (make-collect-element
      #f null
      (lambda (ri)
        (collect-put! ri '(exporting-libraries #f) libs)
        (collect-put! ri '(exporting-packages #f) pkgs))))
    (make-part-collect-decl
     (make-exporting-libraries #f null (and (pair? libs) libs) source-libs pkgs)))))

#lang racket/base
;; Pruned walker that tracks local dependencies in every phase, using the
;; declarations already in the current namespace.
(require syntax/modresolve racket/path racket/list)
(provide local-graph local-deps module-file)

;; A module name is a complete path or (list* 'submod path (listof symbol)).
(define (module-file m) (if (pair? m) (cadr m) m))

;; A graph is (hash module-name (listof module-name)): its keys are the local
;; modules reached, each mapped to everything it imports at every phase
;; (including for-label). Non-local imports appear only as leaves, never as
;; keys. A local import that isn't declared is a key with no imports.

;; local-graph : (listof module-name) path-string -> graph
;; Precondition: every module in `mods` is declared in `current-namespace`
;; (e.g., the document has been loaded). Follows `module->imports` from `mods`
;; into the modules under `root`. Only the submodules that are actually
;; imported are followed. Nothing is loaded or compiled.
(define (local-graph mods root)
  (unless (and (path-string? root) (directory-exists? root))
    (raise-argument-error 'local-graph "existing directory path" 1 mods root))
  (define starts
    (for/list ([m (in-list mods)])
      (define m* (cond [(path-string? m) (simple-form-path m)]
                       [(and (pair? m) (eq? (car m) 'submod) (pair? (cdr m))
                             (path-string? (cadr m)))
                        (list* 'submod (simple-form-path (cadr m)) (cddr m))]
                       [else #f]))
      (unless (and m* (module-declared? m* #f))
        (raise-argument-error 'local-graph "(listof declared module name)" 0 mods root))
      m*))

  (define root-elems (explode-path (simple-form-path root)))
  (define (local? m) (list-prefix? root-elems (explode-path (module-file m))))

  ;; imports-of : module-name -> (listof module-name)
  (define (imports-of m)
    (if (module-declared? m #f)
        (remove-duplicates
         (for*/list ([phase+mpis (in-list (module->imports m))]
                     [mpi (in-list (cdr phase+mpis))]
                     ;; Resolve relative to the file; `mpi` itself carries the
                     ;; submodule context needed for (submod ".." ...) imports.
                     [p (in-value (resolve-module-path-index mpi (module-file m)))]
                     ;; a symbol is a primitive module
                     #:unless (symbol? p))
           (if (path? p)
               (simple-form-path p)
               (list* 'submod (simple-form-path (cadr p)) (cddr p)))))
        '()))

  (for/fold ([graph (hash)]) ([start (in-list starts)])
    (let loop ([graph graph] [m start])
      (if (hash-has-key? graph m)
          graph
          (let ([imports (imports-of m)])
            (for/fold ([graph (hash-set graph m imports)])
                      ([i (in-list imports)] #:when (local? i))
              (loop graph i)))))))

;; local-deps : (listof module-name) path-string -> (listof path)
;; The files of the local modules that `mods` reach; see `local-graph`.
(define (local-deps mods root)
  (remove-duplicates (map module-file (hash-keys (local-graph mods root)))))

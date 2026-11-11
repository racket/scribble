#lang racket/base
;; Pruned walker that tracks local dependencies in every phase, using the
;; declarations already in the current namespace.
(require syntax/modresolve racket/path racket/list)
(provide local-deps)

;; A module name is a complete path or (list* 'submod path (listof symbol)).
(define (module-file m) (if (pair? m) (cadr m) m))

;; local-deps : (listof module-name) path-string -> (listof path)
;; Precondition: every module in `mods` is declared in `current-namespace`
;; (e.g., the document has been loaded). Follows `module->imports` at every
;; phase, including for-label, and returns the files of the reachable modules
;; under `root`. Only the submodules that are actually imported are followed.
;; Nothing is loaded or compiled; an import that isn't declared is kept as a
;; leaf.
(define (local-deps mods root)
  (unless (and (path-string? root) (directory-exists? root))
    (raise-argument-error 'local-deps "existing directory path" 1 mods root))
  (define starts
    (for/list ([m (in-list mods)])
      (define m* (cond [(path-string? m) (simple-form-path m)]
                       [(and (pair? m) (eq? (car m) 'submod) (pair? (cdr m))
                             (path-string? (cadr m)))
                        (list* 'submod (simple-form-path (cadr m)) (cddr m))]
                       [else #f]))
      (unless (and m* (module-declared? m* #f))
        (raise-argument-error 'local-deps "(listof declared module name)" 0 mods root))
      m*))

  (define root-elems (explode-path (simple-form-path root)))
  (define (local? p) (list-prefix? root-elems (explode-path p)))

  (define seen (make-hash))
  (for ([start (in-list starts)])
    (let loop ([m start])
      (unless (hash-ref seen m #f)
        (hash-set! seen m #t)
        (when (module-declared? m #f)
          (for* ([phase+mpis (in-list (module->imports m))]
                 [mpi (in-list (cdr phase+mpis))])
            ;; Resolve relative to the file; `mpi` itself carries the submodule
            ;; context needed for (submod ".." ...) imports.
            (define p (resolve-module-path-index mpi (module-file m)))
            (define m* (cond [(path? p) (simple-form-path p)]
                             [(pair? p) (list* 'submod (simple-form-path (cadr p)) (cddr p))]
                             ; symbol: primitive module
                             [else #f]))
            (when (and m* (local? (module-file m*)))
              (loop m*)))))))
  (remove-duplicates (for/list ([m (in-hash-keys seen)]
                                #:when (local? (module-file m)))
                       (module-file m))))

#lang racket/base
;; Pruned walker that tracks local dependency in every phase.
(require syntax/modcode syntax/modresolve racket/path racket/list)
(provide local-deps)

;; all-imports : compiled-module-expression -> (listof module-path-index)
;; Imports at every phase of `code` and of all its (nested) submodules.
(define (all-imports code)
  (append (append-map cdr (module-compiled-imports code))
          (append-map all-imports
                      (append (module-compiled-submodules code #t)
                              (module-compiled-submodules code #f)))))

;; local-deps : path-string path-string -> (listof path)
;; file must be an existing file, and root must be a project root
(define (local-deps file root)
  (unless (and (path-string? file) (file-exists? file))
    (raise-argument-error 'local-deps "existing file path" 0 file root))
  (unless (and (path-string? root) (directory-exists? root))
    (raise-argument-error 'local-deps "existing directory path" 1 file root))

  (define root-elems (explode-path (simple-form-path root)))
  (define (local? p) (list-prefix? root-elems (explode-path p)))

  (define start (simple-form-path file))

  (define seen (make-hash))
  (let loop ([path start])
    (unless (hash-ref seen path #f)
      (hash-set! seen path #t)
      (for ([mpi (in-list (all-imports (get-module-code path)))])
        (define p (resolve-module-path-index mpi path))
        (define file (cond [(path? p) (simple-form-path p)]
                           ; (submod path name ...): the whole file is walked anyway
                           [(pair? p) (simple-form-path (cadr p))]
                           ; symbol: primitive module
                           [else #f]))
        (when (and file (local? file))
          (loop file)))))
  (hash-keys seen))

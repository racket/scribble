#lang racket/base
;; Pruned, phase-insensitive dependency walker.
(require syntax/modcode syntax/modresolve racket/path racket/list)
(provide local-deps)

;; local-deps : path-string path-string -> (listof path)
;; file must be an existing file, and root must be a project root
(define (local-deps file root)
  (unless (and (path-string? file) (file-exists? file))
    (raise-argument-error 'local-deps "existing file path" 0 file root))
  (unless (and (path-string? root) (directory-exists? root))
    (raise-argument-error 'local-deps "existing directory path" 1 file root))
  (define start (simple-form-path file))
  (define root-elems (explode-path (simple-form-path root)))
  (define (local? p) (list-prefix? root-elems (explode-path p)))
  (define seen (make-hash))
  (let loop ([r start])
    (define plain (if (pair? r) (cadr r) r))
    (unless (hash-ref seen r #f)
      (hash-set! seen r #t)
      (define code (get-module-code plain #:submodule-path (if (pair? r) (cddr r) '())))
      (for* ([phase+mpis (in-list (module-compiled-imports code))]
             [mpi (in-list (cdr phase+mpis))])
        (define p (resolve-module-path-index mpi plain))
        (define p* (cond [(path? p) (simple-form-path p)]
                         [(pair? p) (list* 'submod (simple-form-path (cadr p)) (cddr p))]
                         ; symbol: primitive module
                         [else #f]))
        (when (and p* (local? (if (pair? p*) (cadr p*) p*)))
          (loop p*)))))
  (remove-duplicates (for/list ([k (in-hash-keys seen)]) (if (pair? k) (cadr k) k))))

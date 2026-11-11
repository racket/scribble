#lang racket/base

;; Tests for `local-deps`, which collects the files a loaded document depends on.

(require rackunit
         racket/path
         racket/runtime-path
         scribble/private/walk)

(define-runtime-path docs-dir "local-deps-docs")
(define proj-dir (build-path docs-dir "proj"))
(define main-file (build-path proj-dir "main.scrbl"))

;; doc-modules : path-string -> (listof module-name)
;; The document module and its `doc` submodule, whichever are declared.
(define (doc-modules file)
  (define p (simple-form-path file))
  (filter (lambda (m) (module-declared? m #f)) (list p `(submod ,p doc))))

;; walk : path-string path-string -> (listof path)
;; Load `file` the way `scribble` does (the `doc` submodule if there is one) in a
;; fresh namespace, then walk the declared modules.
(define (walk file root)
  (parameterize ([current-namespace (make-base-namespace)])
    (define p (simple-form-path file))
    (if (module-declared? `(submod ,p doc) #t)
        (dynamic-require `(submod ,p doc) #f)
        (dynamic-require p #f))
    (local-deps (doc-modules file) root)))

(define (sort-rel-names paths)
  (sort (for/list ([p (in-list paths)])
          (path->string (find-relative-path (simple-form-path docs-dir) p)))
        string<?))

(test-case "bad arguments are rejected"
  (parameterize ([current-namespace (make-base-namespace)])
    (check-exn exn:fail:contract?                     ; not declared
               (lambda () (local-deps (list (simple-form-path main-file)) proj-dir)))
    (check-exn exn:fail:contract?                     ; not a module name
               (lambda () (local-deps (list 'racket/base) proj-dir)))
    (dynamic-require main-file #f)
    (check-exn exn:fail:contract?                     ; root does not exist
               (lambda () (local-deps (doc-modules main-file) (build-path docs-dir "missing"))))
    (check-exn exn:fail:contract?                     ; a file is not a root
               (lambda () (local-deps (doc-modules main-file) main-file)))))

(define proj-files
  '("proj/api.rkt" "proj/ch1.scrbl" "proj/ch2.scrbl" "proj/common.rkt"
    "proj/lp.scrbl" "proj/main.scrbl" "proj/sub/s1.scrbl"))

(test-case "only files under the root are reported"
  (check-equal? (sort-rel-names (walk main-file proj-dir)) proj-files))

(test-case "a wider root reaches files outside the document's folder"
  (check-equal? (sort-rel-names (walk main-file docs-dir))
                (sort (list* "lib/util.rkt" "projish/x.rkt" proj-files) string<?)))

(test-case "a shared dependency is reported once"
  (define deps (sort-rel-names (walk main-file proj-dir)))
  (check-equal? (length (filter (lambda (f) (equal? f "proj/common.rkt")) deps)) 1))

(test-case "results are complete, simplified paths"
  (for ([p (in-list (walk main-file proj-dir))])
    (check-true (complete-path? p))
    (check-equal? p (simple-form-path p))))

(test-case "equivalent spellings of the module and root give the same result"
  (check-equal? (sort-rel-names (walk (build-path proj-dir "sub" 'up "main.scrbl")
                                      (build-path proj-dir "sub" 'up)))
                proj-files)
  (parameterize ([current-directory proj-dir])
    (check-equal? (sort-rel-names (walk "main.scrbl" ".")) proj-files)))

(test-case "a leaf document depends only on itself"
  (check-equal? (sort-rel-names (walk (build-path proj-dir "sub" "s1.scrbl") proj-dir))
                '("proj/sub/s1.scrbl")))

;; Submodules
(define (subs-deps name)
  (sort-rel-names (walk (build-path docs-dir name) docs-dir)))

(test-case "requires inside imported submodules are followed"
  ;; subs-main.scrbl imports (submod "subs-helpers.rkt" extra), which reaches
  ;; subs-extra-dep.rkt and, through its nested `inner`, subs-nested-dep.rkt
  (check-not-false (member "subs-extra-dep.rkt" (subs-deps "subs-main.scrbl")))
  (check-not-false (member "subs-nested-dep.rkt" (subs-deps "subs-main.scrbl"))))

(test-case "submodules that nothing imports are not followed"
  ;; subs-helpers.rkt's `test` submodule requires subs-test-only.rkt
  (check-false (member "subs-test-only.rkt" (subs-deps "subs-main.scrbl")))
  (check-equal? (subs-deps "subs-helpers.rkt") '("subs-helpers.rkt")))

(test-case "an lp2 document's requires are found through its doc submodule"
  (check-equal? (subs-deps "subs-lp.scrbl")
                '("subs-lp-dep.rkt" "subs-lp.scrbl")))

(test-case "a (submod file ...) dependency is reported as its file, once"
  (check-equal? (subs-deps "subs-main.scrbl")
                '("subs-extra-dep.rkt" "subs-helpers.rkt" "subs-lp-dep.rkt"
                  "subs-lp.scrbl" "subs-main.scrbl" "subs-nested-dep.rkt")))

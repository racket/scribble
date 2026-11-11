#lang racket/base

;; Tests for `local-deps`, which collects the files a document depends on.

(require rackunit
         racket/path
         racket/runtime-path
         scribble/private/walk)

(define-runtime-path docs-dir "local-deps-docs")
(define proj-dir (build-path docs-dir "proj"))
(define main-file (build-path proj-dir "main.scrbl"))

(test-case "bad arguments are rejected"
  (check-exn exn:fail:contract?
             (lambda () (local-deps (build-path proj-dir "missing.scrbl") proj-dir)))
  (check-exn exn:fail:contract?
             (lambda () (local-deps main-file (build-path docs-dir "missing"))))
  (check-exn exn:fail:contract?
             (lambda () (local-deps proj-dir proj-dir)))   ; a directory is not a file
  (check-exn exn:fail:contract?
             (lambda () (local-deps main-file main-file)))) ; a file is not a root

(define (sort-rel-names paths)
  (sort (for/list ([p (in-list paths)])
          (path->string (find-relative-path (simple-form-path docs-dir) p)))
        string<?))

(define proj-files
  '("proj/api.rkt" "proj/ch1.scrbl" "proj/ch2.scrbl" "proj/common.rkt"
    "proj/lp.scrbl" "proj/main.scrbl" "proj/sub/s1.scrbl"))

(test-case "only files under the root are reported"
  (check-equal? (sort-rel-names (local-deps main-file proj-dir)) proj-files))

(test-case "a wider root reaches files outside the document's folder"
  (check-equal? (sort-rel-names (local-deps main-file docs-dir))
                (sort (list* "lib/util.rkt" "projish/x.rkt" proj-files) string<?)))

(test-case "a shared dependency is reported once"
  (define deps (sort-rel-names (local-deps main-file proj-dir)))
  (check-equal? (length (filter (lambda (f) (equal? f "proj/common.rkt")) deps)) 1))

(test-case "results are complete, simplified paths"
  (for ([p (in-list (local-deps main-file proj-dir))])
    (check-true (complete-path? p))
    (check-equal? p (simple-form-path p))))

(test-case "equivalent spellings of the file and root give the same result"
  (check-equal? (sort-rel-names (local-deps (build-path proj-dir "sub" 'up "main.scrbl")
                                       (build-path proj-dir "sub" 'up)))
                proj-files)
  (parameterize ([current-directory proj-dir])
    (check-equal? (sort-rel-names (local-deps "main.scrbl" ".")) proj-files)))

(test-case "a leaf document depends only on itself"
  (check-equal? (sort-rel-names (local-deps (build-path proj-dir "sub" "s1.scrbl") proj-dir))
                '("proj/sub/s1.scrbl")))

;; Submodules
(define (subs-deps name)
  (sort-rel-names (local-deps (build-path docs-dir name) docs-dir)))

(test-case "requires inside submodules are followed"
  ;; subs-helpers.rkt reaches subs-extra-dep.rkt from `module`, subs-nested-dep.rkt
  ;; from a nested submodule, and subs-test-only.rkt from `module+`
  (check-equal? (subs-deps "subs-helpers.rkt")
                '("subs-extra-dep.rkt" "subs-helpers.rkt"
                  "subs-nested-dep.rkt" "subs-test-only.rkt")))

(test-case "an lp2 document's requires are found from the document itself"
  (check-equal? (subs-deps "subs-lp.scrbl")
                '("subs-lp-dep.rkt" "subs-lp.scrbl")))

(test-case "a (submod file ...) dependency is reported as its file, once"
  (check-equal? (subs-deps "subs-main.scrbl")
                '("subs-extra-dep.rkt" "subs-helpers.rkt" "subs-lp-dep.rkt"
                  "subs-lp.scrbl" "subs-main.scrbl" "subs-nested-dep.rkt"
                  "subs-test-only.rkt")))


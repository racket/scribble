#lang racket/base
;; Re-render a document whenever it or one of its local dependencies changes.
(require racket/exn
         racket/list
         racket/path
         racket/rerequire
         syntax/modresolve
         "build.rkt"
         "walk.rkt")
(provide watch-doc)

;; watch-doc : string [#:on-watch ((listof path) -> any)] [#:stop evt?] -> void
;; Loads (with `mk-doc` and `dynamic-rerequire`) and renders (with `build-docs`,
;; in whatever format the parameters select) `file` at the start, then again
;; after every change to it or to a local dependency, i.e. one in the
;; document's folder. `on-watch` gets the
;; watched files whenever the watcher starts waiting; the loop ends when `stop`
;; is ready.
(define (watch-doc file
                   #:on-watch [on-watch void]
                   #:stop [stop never-evt])
  (define doc-path
    (simple-form-path (if (current-lib-mode)
                          (resolve-module-path `(lib ,file) #f)
                          file)))
  (define root (path-only doc-path))
  (define root-elems (explode-path root))
  ;; The document module and its `doc` submodule; `mk-doc` loads one of them.
  (define doc-mods (list doc-path `(submod ,doc-path ,(current-doc-binding))))
  (define (load)
    (mk-doc file (lambda (mp) (dynamic-rerequire mp #:verbosity 'reload))))
  ;; render : any -> void
  ;; Renders the document, printing the error if that fails.
  (define (render doc)
    (with-handlers ([exn:fail? (lambda (e) (eprintf "~a" (exn->string e)))])
      (build-docs (list doc) (list file))))

  ;; error-files : exn -> (listof path)
  ;; The local files an error's source locations point to (for a read or
  ;; syntax error, the broken file).
  (define (error-files e)
    (if (exn:srclocs? e)
        (for*/list ([loc (in-list ((exn:srclocs-accessor e) e))]
                    [src (in-value (and (srcloc? loc) (srcloc-source loc)))]
                    #:when (path? src)
                    [p (in-value (simple-form-path src))]
                    #:when (list-prefix? root-elems (explode-path p)))
          p)
        '()))

  ;; load-doc : -> (values any (listof path))
  ;; The document, or #f (after printing the error) along with the local files
  ;; the error points to.
  (define (load-doc)
    (with-handlers ([exn:fail? (lambda (e)
                                 (eprintf "~a" (exn->string e))
                                 (values #f (error-files e)))])
      (values (load) '())))

  ;; deps-of : (listof module-name) -> (listof path)
  ;; The files to watch for `mods`: the local dependencies of the declared
  ;; ones, read from their declarations, plus the undeclared ones that are
  ;; files (e.g. a document that has never loaded, or a file named by a load
  ;; error), so fixing them triggers a reload. After a failed reload, the
  ;; declarations are the last ones that loaded, so the previous dependencies
  ;; are kept.
  (define (deps-of mods)
    (define-values (declared undeclared)
      (partition (lambda (m) (module-declared? m #f)) mods))
    (remove-duplicates (append (local-deps declared root)
                               (filter path? undeclared))))

  ;; Load and render the document once, which also declares its dependencies so
  ;; they can be watched from the start.
  (define-values (doc err-files) (load-doc))
  (when doc (render doc))
  ;; `watches` maps each watched file to its change evt (#f while the file is
  ;; missing).
  (let loop ([watches (arm-evts (deps-of (append doc-mods err-files)))])
    (define evts (filter values (hash-values watches)))
    (on-watch (hash-keys watches))
    (cond
      [(null? evts)
       ;; Every file is momentarily missing: wait and retry instead of blocking.
       (if (sync/timeout 0.1 stop)
           (void)
           (loop (arm-evts (hash-keys watches) watches)))]
      [(sync (wrap-evt stop (lambda (_) #t))
             (wrap-evt (apply choice-evt evts) (lambda (_) #f)))
       (cancel-evts watches)]
      [else
       (cancel-evts watches)
       ;; Watch again before `load-doc` reads anything: an evt only sees changes
       ;; made after it exists, so a save during the reload makes the next
       ;; wait return at once and triggers another rebuild.
       (define pending (arm-evts (hash-keys watches)))
       (define-values (doc err-files) (load-doc))
       (when doc (render doc))
       ;; Reusing `pending` keeps the evts that fired during the reload.
       (define next (arm-evts (deps-of (append doc-mods err-files)) pending))
       (cancel-evts pending next)
       (loop next)])))

;; arm-evts : (listof path) [(hash path (or/c evt #f))] -> (hash path (or/c evt #f))
;; A change evt for each path, reusing the ones in `have`. A path whose file is
;; briefly missing (e.g. during a rename-based save) maps to #f and is retried
;; on the next call.
(define (arm-evts paths [have (hash)])
  (for/hash ([p (in-list paths)])
    (values p (or (hash-ref have p #f)
                  (filesystem-change-evt p (lambda () #f))))))

;; cancel-evts : (hash path (or/c evt #f)) [hash] -> void
;; Cancels the evts of the paths that are not keys of `keep`.
(define (cancel-evts watches [keep (hash)])
  (for ([(p e) (in-hash watches)]
        #:when (and e (not (hash-has-key? keep p))))
    (filesystem-change-evt-cancel e)))

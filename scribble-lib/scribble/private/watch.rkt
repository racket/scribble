#lang racket/base
;; Re-render a document whenever it or one of its local dependencies changes.
(require compiler/cm
         racket/exn
         racket/list
         racket/path
         racket/set
         syntax/modresolve
         "build.rkt"
         "walk.rkt")
(provide watch-doc
         cm-rebuild)

;; The modules whose structs the renderers dispatch on. Every build's namespace
;; shares them with the namespace `watch-doc` starts in, so that a document's
;; parts and style properties are the ones the renderer knows.
(define shared-modules
  '(scribble/core
    scribble/html-properties
    scribble/latex-properties
    scribble/typst-properties
    scribble/render-struct
    scribble/decode-struct
    scribble/manual-struct
    scribble/struct))

;; doc-path-of : string -> path
;; The complete path of the document `file` names (a `lib` path in lib mode).
(define (doc-path-of file)
  (simple-form-path (if (current-lib-mode)
                        (resolve-module-path `(lib ,file) #f)
                        file)))


;; watch-doc : string [#:on-watch ((listof path) -> any)] -> bottom

;; Start a watcher to watch changes to `file` and all its dependency. The
;; watcher uses `ccm-rebuild` to compile and load their modules, and it then
;; renders the outputs with `build-docs`

;; An `on-watch` callback gets the watched files whenever the watcher starts waiting.
(define (watch-doc file
                   #:on-watch [on-watch void])
  (define doc-path (doc-path-of file))
  (define root (path-only doc-path))
  (define root-elems (explode-path root))
  (define base-ns (current-namespace))

  (define (fresh-namespace)
    (define ns (make-base-empty-namespace))
    (parameterize ([current-namespace base-ns])
      (for ([m (in-list shared-modules)]
            #:when (module-declared? m #f))
        (namespace-attach-module base-ns m ns)))
    ns)

  (define (render doc)
    (with-handlers ([exn:fail? (lambda (e) (eprintf "~a" (exn->string e)))])
      (build-docs (list doc) (list file))))

  (define (error-files e)
    (if (exn:srclocs? e)
        (for*/list ([loc (in-list ((exn:srclocs-accessor e) e))]
                    [src (in-value (and (srcloc? loc) (srcloc-source loc)))]
                    #:when (path? src)
                    [p (in-value (simple-form-path src))]
                    #:when (list-prefix? root-elems (explode-path p)))
          p)
        '()))

  ;; build :: (maybe namespace) -> (maybe namespace) -> Boolean -> (Values namespace graph)
  (define (build prev-ns prev-graph changed)
    (define ns (fresh-namespace))
    (with-handlers ([exn:fail? (lambda (e)
                                 (eprintf "~a" (exn->string e))
                                 (values #f prev-graph (error-files e)))])
      (define-values (doc graph) (cm-rebuild file root ns prev-ns prev-graph changed))
      (render doc)
      (values ns graph '())))

  ;; watch-files : graph (listof path) -> (listof path)
  ;; The files to watch: those of the graph's modules (the document's file if
  ;; it has never loaded) and the files a load error points to, so fixing them
  ;; triggers a rebuild.
  (define (watch-files graph err-files)
    (remove-duplicates
     (append (if (hash-empty? graph)
                 (list doc-path)
                 (map module-file (hash-keys graph)))
             err-files)))

  ;; The first build also declares the dependencies, so they can be watched
  ;; from the start.
  (define-values (ns0 graph0 err-files0) (build #f (hash) '()))
  ;; `watches` maps each watched file to its change evt (#f while the file is
  ;; missing). `ns` and `graph` are those of the last successful build (#f and
  ;; empty before one), and `pending` holds the files changed since then.
  (let loop ([watches (arm-evts (watch-files graph0 err-files0))]
             [ns ns0]
             [graph graph0]
             [pending '()])
    (define evts (filter values (hash-values watches)))
    (on-watch (hash-keys watches))
    (cond
      [(null? evts)
       ;; Every file is momentarily missing: wait and retry instead of blocking.
       (sleep 0.1)
       (loop (arm-evts (hash-keys watches) watches) ns graph pending)]
      [else
       (sync (apply choice-evt evts))
       (define changed (remove-duplicates (append pending (changed-files watches))))
       (cancel-evts watches)
       ;; Watch again before building: an evt only sees changes made after it
       ;; exists, so a save during the build makes the next wait return at once
       ;; and triggers another build.
       (define armed (arm-evts (hash-keys watches)))
       (define-values (new-ns new-graph err-files) (build ns graph changed))
       ;; Reusing `armed` keeps the evts that fired during the build.
       (define next (arm-evts (watch-files new-graph err-files) armed))
       (cancel-evts armed next)
       (if new-ns
           (loop next new-ns new-graph '())
           ;; Keep the changed files pending, so a module that broke this build
           ;; is not reused from `ns` by the next one.
           (loop next ns graph changed))])))

;; cm-rebuild : string path namespace (or/c #f namespace) graph (listof path)
;;           -> (values any graph)
;; Loads the document `file`, whose local dependencies are under `root`, into
;; `new-ns` and returns it together with its local dependency graph in
;; `new-ns`. `prev-ns` (with its graph `prev-graph`) is the namespace of the
;; previous build, or #f; `changed` are the files changed since then.
;;
;; The modules of `prev-ns` that don't depend on a changed file, and the
;; non-local modules, are attached to `new-ns` and so reused as is. The rest
;; are loaded with a compilation manager, which recompiles every changed file
;; and every dependent whose dependencies changed (e.g. a module using a
;; changed macro), and writes their compiled files. Its checks skip files
;; outside `root`. Raises an exception if the document fails to load.
(define (cm-rebuild file root new-ns prev-ns prev-graph changed)
  (define root-elems (explode-path (simple-form-path root)))
  (define (local? p) (list-prefix? root-elems (explode-path (simple-form-path p))))
  (when prev-ns
    (define dirty (dirty-files prev-graph (map simple-form-path changed)))
    (define clean
      (for/list ([m (in-hash-keys prev-graph)]
                 #:unless (set-member? dirty (module-file m)))
        m))
    (define libraries
      (for*/list ([imports (in-hash-values prev-graph)]
                  [i (in-list imports)]
                  #:unless (hash-has-key? prev-graph i))
        i))
    (for ([m (in-list (remove-duplicates (append clean libraries)))])
      (attach-module prev-ns m new-ns)))
  (define doc-path (doc-path-of file))
  (parameterize ([current-namespace new-ns])
    (parameterize* ([manager-skip-file-handler
                     (lambda (p)
                       (and (not (local? p))
                            (file-stamp-in-paths p (list (path-only p)))))]
                    [current-doc-load-handler
                     ;; created in `new-ns`: a handler only compiles in the
                     ;; namespace it was created in
                     (make-compilation-manager-load/use-compiled-handler)])
      (define doc (mk-doc file))
      (define doc-mods
        (filter (lambda (m) (module-declared? m #f))
                (list doc-path `(submod ,doc-path ,(current-doc-binding)))))
      (values doc (local-graph doc-mods root)))))

;; dirty-files : graph (listof path) -> (set path)
(define (dirty-files graph changed)
  ;; importers : (hash path (listof path)), from a file to the files that
  ;; import one of its modules
  (define importers
    (for*/fold ([importers (hash)])
               ([(m imports) (in-hash graph)]
                [i (in-list imports)]
                #:when (hash-has-key? graph i))
      (hash-update importers (module-file i)
                   (lambda (files) (cons (module-file m) files))
                   '())))
  (let loop ([todo changed] [dirty (set)])
    (cond
      [(null? todo) dirty]
      [(set-member? dirty (car todo)) (loop (cdr todo) dirty)]
      [else (loop (append (hash-ref importers (car todo) '()) (cdr todo))
                  (set-add dirty (car todo)))])))

(define (attach-module from m to)
  (with-handlers ([exn:fail:contract?
                   (lambda (e) (namespace-attach-module-declaration from m to))])
    (namespace-attach-module from m to)))

(define (changed-files watches)
  (for/list ([(p e) (in-hash watches)]
             #:when (or (not e) (sync/timeout 0 e)))
    p))

(define (arm-evts paths [have (hash)])
  (for/hash ([p (in-list paths)])
    (values p (or (hash-ref have p #f)
                  (filesystem-change-evt p (lambda () #f))))))

(define (cancel-evts watches [keep (hash)])
  (for ([(p e) (in-hash watches)]
        #:when (and e (not (hash-has-key? keep p))))
    (filesystem-change-evt-cancel e)))

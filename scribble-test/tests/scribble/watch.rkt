#lang racket/base

;; Tests for `watch-doc`, which re-renders a document when it or one of its local
;; dependencies changes. Runs the watcher on a copy of local-deps-docs/ and edits
;; files in it.

(require rackunit
         racket/async-channel
         racket/file
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         scribble/private/build
         scribble/private/watch
         (prefix-in text: scribble/text-render))

(define-runtime-path docs-dir "local-deps-docs")

;; How long to wait for the watcher before failing.
(define timeout 30)

;; with-watcher : procedure [#:before-start ((string string string -> void) -> any)] -> void
;; Copies the fixtures to a temporary directory and starts `watch-doc` on
;; proj/main.scrbl in a fresh namespace, rendering text output into a temporary
;; directory. Then calls `body` with:
;;  - `edit!` : rel-path old new -> void, which replaces `old` with `new` in a file;
;;  - `expect-render` : (or/c string (string -> any)) -> (listof string), which
;;    waits for a render made after the last edit whose text contains the given
;;    marker (or satisfies the predicate), and returns the files watched after
;;    it (relative to proj/);
;;  - `next-watched` : -> (listof string), which waits for the watcher to start
;;    waiting and returns the watched files;
;;  - `rendered` : -> (or/c string #f), the current rendered text, if any;
;;  - `proj`, the copied proj/ directory.
;; `before-start` gets `edit!` and runs before the watcher starts.
(define (with-watcher body #:before-start [before-start void])
  (define tmp (make-temporary-directory))
  (for ([d (in-list '("proj" "lib" "projish"))])
    (copy-directory/files (build-path docs-dir d) (build-path tmp d)))
  (define proj (build-path tmp "proj"))
  (define out-dir (build-path tmp "out"))
  (define out-file (build-path out-dir "main.txt"))
  (define watched (make-async-channel))
  (define stop (make-semaphore))
  (define last-edit 0)
  (define (edit! rel old new)
    (define p (build-path proj rel))
    (define s (file->string p))
    (unless (string-contains? s old)
      (error 'edit! "~a does not contain ~s" rel old))
    ;; `dynamic-rerequire` compares modification times in whole seconds, so an
    ;; edit within the same second as the previous load would not be reloaded.
    ;; This also makes renders from before the edit older than `last-edit`.
    (sleep 1.1)
    (set! last-edit (current-seconds))
    (call-with-output-file p #:exists 'truncate
      (lambda (o) (write-string (string-replace s old new) o))))
  (define (rendered)
    (and (file-exists? out-file) (file->string out-file)))
  (define (next-watched)
    (define files (sync/timeout timeout watched))
    (unless files (fail "the watcher did not start waiting again"))
    (sort (for/list ([f (in-list files)])
            (path->string (find-relative-path (simple-form-path proj) f)))
          string<?))
  ;; The watcher starts waiting again after every render, so check the output
  ;; each time it does.
  (define (expect-render want)
    (define ok? (if (string? want) (lambda (t) (string-contains? t want)) want))
    (let loop ()
      (define files (next-watched))
      (define text (rendered))
      (if (and text
               (>= (file-or-directory-modify-seconds out-file) last-edit)
               (ok? text))
          files
          (loop))))
  (before-start edit!)
  ;; Share this module's instances of the renderer (and so scribble/core), so the
  ;; document's structs are the ones the renderer knows.
  (define ns (make-base-namespace))
  (define this-ns (variable-reference->namespace (#%variable-reference)))
  (namespace-attach-module this-ns 'scribble/private/watch ns)
  (namespace-attach-module this-ns 'scribble/text-render ns)
  (define watcher
    (parameterize ([current-namespace ns]
                   [current-error-port (open-output-nowhere)]
                   [current-render-mixin text:render-mixin]
                   [current-dest-directory out-dir]
                   [current-quiet #t])
      (thread
       (lambda ()
         (watch-doc (path->string (build-path proj "main.scrbl"))
                    #:on-watch (lambda (files) (async-channel-put watched files))
                    #:stop (semaphore-peek-evt stop))))))
  (dynamic-wind
   void
   (lambda () (body edit! expect-render next-watched rendered proj) (void))
   (lambda ()
     (semaphore-post stop)
     (unless (sync/timeout timeout watcher) (kill-thread watcher))
     (delete-directory/files tmp))))

(define main-deps
  '("api.rkt" "ch1.scrbl" "ch2.scrbl" "common.rkt" "lp.scrbl" "main.scrbl" "sub/s1.scrbl"))

(test-case "edits to the document and its dependencies are re-rendered"
  (with-watcher
   (lambda (edit! expect-render next-watched rendered proj)
     (check-equal? (next-watched) main-deps)
     (edit! "main.scrbl" "@title{Main}" "@title{Main MARK0}")
     (expect-render "MARK0")
     (edit! "ch1.scrbl" "\"one\"" "\"one MARK1\"")               ; include-section
     (expect-render "MARK1")
     (edit! "sub/s1.scrbl" "A nested section." "A nested MARK2.") ; nested include
     (expect-render "MARK2")
     (edit! "common.rkt" "(italic s)" "(italic s \" MARK3\")")    ; helper module
     (expect-render "MARK3")
     (edit! "lp.scrbl" "@title{Literate}" "@title{Literate MARK4}") ; lp2 doc submodule
     (expect-render "MARK4")
     ;; for-label only: the text can't show it, so just check a new render happens
     (edit! "api.rkt" "(define (f) 1)" "(define (f) 2)")
     (expect-render "MARK4"))))

(test-case "a syntax error is survived and fixing it re-renders"
  (with-watcher
   (lambda (edit! expect-render next-watched rendered proj)
     (next-watched)
     (edit! "ch1.scrbl" "@title{Chapter 1}" "@title{Chapter 1} @(")
     ;; no render, but the watcher keeps going (and still watches ch1.scrbl)
     (check-equal? (next-watched) main-deps)
     (check-false (rendered))
     (edit! "ch1.scrbl" "@title{Chapter 1} @(" "@title{Chapter 1 FIXED}")
     (expect-render "FIXED"))))

(test-case "added dependencies are watched and removed ones are dropped"
  (with-watcher
   (lambda (edit! expect-render next-watched rendered proj)
     (next-watched)
     (call-with-output-file (build-path proj "ch3.scrbl")
       (lambda (o) (write-string "#lang scribble/manual\n@title{Chapter 3}\nThree.\n" o)))
     (edit! "main.scrbl" "@include-section[\"ch2.scrbl\"]"
            "@include-section[\"ch2.scrbl\"]\n@include-section[\"ch3.scrbl\"]")
     (check-not-false (member "ch3.scrbl" (expect-render "Chapter 3")))
     (edit! "ch3.scrbl" "Three." "Three MARK6.")
     (expect-render "MARK6")
     (edit! "main.scrbl" "\n@include-section[\"ch3.scrbl\"]" "")
     (check-false (member "ch3.scrbl"
                          (expect-render (lambda (t) (not (string-contains? t "Chapter 3")))))))))

(test-case "a document broken from the start recovers once fixed"
  (with-watcher
   #:before-start
   (lambda (edit!) (edit! "ch1.scrbl" "@title{Chapter 1}" "@title{Chapter 1} @("))
   (lambda (edit! expect-render next-watched rendered proj)
     ;; main.scrbl never loaded, so only it and the broken file are watched
     (check-equal? (next-watched) '("ch1.scrbl" "main.scrbl"))
     (edit! "ch1.scrbl" "@title{Chapter 1} @(" "@title{Chapter 1 FIXED}")
     (check-equal? (expect-render "FIXED") main-deps)
     (edit! "common.rkt" "(italic s)" "(italic s \" MARK3\")")
     (expect-render "MARK3"))))

#lang racket/base

(require "struct.rkt"
         "basic.rkt"
         syntax/modcode)

(provide find-racket-tag
         (rename-out [find-racket-tag find-scheme-tag]))

(define module-info-cache (make-hasheq))

(define (module-path-index-rejoin mpi rel-to)
  (define-values (name base)
    (module-path-index-split mpi))
  (cond
    [(not name) rel-to]
    [(not base) mpi]
    [else
     (module-path-index-join name 
                             (module-path-index-rejoin base rel-to))]))

(define (try thunk fail-thunk)
  (with-handlers* ([exn:fail? (lambda (exn) (fail-thunk))])
    (thunk)))

(define (find-racket-tag part ri stx/binding phase-level
                         ;; assume that `stx/binding` has a suitable scope, if any,
                         ;; already; currently used only for the default suffix
                         #:space [space #f]
                         ;; used as part of the tag to find
                         #:suffix [suffix space]
                         #:unlinked-ok? [unlinked-ok? #f])
  ;; The phase-level argument is used only when `stx/binding'
  ;; is an identifier.
  ;;
  ;; Note: documentation keys currently don't distinguish different
  ;; phase definitions of an identifier from a source module.
  ;; That is, there's no way to document (define x ....) differently
  ;; from (define-for-syntax x ...). This isn't a problem in practice,
  ;; because no one uses the same name for different-phase exported
  ;; bindings.
  ;;
  ;; Formerly, we assumed that bindings are defined as originating from some
  ;; module at phase 0. [Maybe it's defined at phase 1 and re-exported
  ;; later for phase 0 (after a require-for-template), in which case the
  ;; re-exporting module is the one we find.] That assumption has been
  ;; lifted, however; search for "GONE" below.
  (define b
    (cond
      [(identifier? stx/binding)
       (identifier-binding stx/binding phase-level)]
      [(and (list? stx/binding)
            (= 7 (length stx/binding)))
       stx/binding]
      [else
       (and (not (symbol? (car stx/binding)))
            (list #f
                  (cadr stx/binding)
                  (car stx/binding)
                  (cadr stx/binding)
                  (if (= 2 (length stx/binding))
                      0
                      (caddr stx/binding))
                  (if (= 2 (length stx/binding))
                      0
                      (cadddr stx/binding))
                  (if (= 2 (length stx/binding))
                      0
                      (cadddr (cdr stx/binding)))))]))
  (cond
    [(not (pair? b)) #f]
    [else
     (define seen (make-hash))
     (define search-key #f)
     (let loop ([queue (list (list (caddr b) (cadddr b) (list-ref b 4) (list-ref b 5) (list-ref b 6)))]
                [rqueue null]
                [need-result? #t])
       (cond
         [(null? queue)
          (if (null? rqueue)
              ;; Not documented
              #f
              (loop (reverse rqueue) null need-result?))]
         [else
          (define mod (list-ref (car queue) 0))
          (define id (list-ref (car queue) 1))
          (define defn-phase (list-ref (car queue) 2))
          (define import-phase (list-ref (car queue) 3))
          (define export-phase (list-ref (car queue) 4))
          (let ([queue (cdr queue)])
            (define rmp (module-path-index-resolve mod))
            (define eb
              (list* (module-path-index->taglet mod)
                     id
                     (if suffix (list suffix) null)))
            (when (not search-key)
              (set! search-key (if unlinked-ok?
                                   (cons #f eb)
                                   eb)))
            (define v (and eb (resolve-search search-key part ri `(dep ,eb))))
            (define here-result
              (and need-result?
                   v
                   (let ([v (resolve-get/tentative part ri `(form ,eb))])
                     (or (and v `(form ,eb))
                         `(def ,eb)))))
            (let ([need-result? (and need-result? (not here-result))])
              (define rmp-name (resolved-module-path-name rmp))
              ;; Even if we've found `here-result', look deeper so that we have 
              ;; consistent `dep' results.
              (define nest-result
                (cond
                  ;; Maybe it's re-exported from this module...
                  ;; Try a shortcut:
                  [(eq? rmp (and (car b) (module-path-index-resolve (car b))))
                   ;; Not defined through this path, so keep looking
                   (loop queue rqueue need-result?)]
                  ;; Check parents, if we can get the source:
                  [(and (or (path? rmp-name)
                            (and (list? rmp-name)
                                 (path? (car rmp-name))))
                        (not (hash-ref seen (cons export-phase rmp) #f)))
                   (define exports
                     (hash-ref
                      module-info-cache
                      rmp
                      (lambda ()
                        (define-values (valss stxess)
                          (try
                           (lambda ()
                             ;; First, try using bytecode:
                             (module-compiled-exports 
                              (get-module-code (if (list? rmp-name)
                                                   (car rmp-name)
                                                   rmp-name)
                                               #:submodule-path (if (list? rmp-name)
                                                                    (cdr rmp-name)
                                                                    '())
                                               #:choose (lambda (src zo so) 'zo))))
                           (lambda ()
                             (try
                              (lambda ()
                                ;; Bytecode not available. Declaration in the
                                ;; current namespace?
                                (module->exports rmp))
                              (lambda ()
                                (values null null))))))
                        (define t
                          ;; Merge the two association lists:
                          (let loop ([base valss]
                                     [stxess stxess])
                            (cond
                              [(null? stxess) base]
                              [(assoc (caar stxess) base)
                               => (lambda (l)
                                    (loop (cons (cons (car l)
                                                      (append (cdar stxess)
                                                              (cdr l)))
                                                (remq l base))
                                          (cdr stxess)))]
                              [else (loop (cons (car stxess)
                                                base)
                                          (cdr stxess))])))
                        (hash-set! module-info-cache rmp t)
                        t)))
                   (hash-set! seen (cons export-phase rmp) #t)
                   (let ([a (assq id (let ([a (assoc export-phase exports)])
                                       (if a
                                           (cdr a) 
                                           null)))])
                     (cond
                       [a
                        (loop queue
                              (append (map (lambda (m)
                                             (if (pair? m)
                                                 (list (module-path-index-rejoin (car m) mod)
                                                       (list-ref m 2)
                                                       defn-phase
                                                       (list-ref m 1)
                                                       (list-ref m 3))
                                                 (list (module-path-index-rejoin m mod)
                                                       id
                                                       defn-phase
                                                       import-phase
                                                       export-phase)))
                                           (reverse (cadr a)))
                                      rqueue)
                              need-result?)]
                       [else
                        ;; A dead end may not be our fault: the files could
                        ;; have changed in inconsistent ways. So just say #f
                        ;; for now.
                        #;
                        (error 'find-racket-tag
                               "dead end when looking for binding source: ~e"
                               id)
                        (loop queue rqueue need-result?)]))]
                  [else
                   ;; Can't get the module source, so continue with queue:
                   (loop queue rqueue need-result?)]))
              (or here-result
                  nest-result)))]))]))

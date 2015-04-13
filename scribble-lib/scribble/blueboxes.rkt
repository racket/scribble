#lang racket/base
(require setup/dirs
         racket/serialize
         racket/contract
         scribble/core)

(provide
 (contract-out
  [fetch-blueboxes-strs (->* (tag?) (#:blueboxes-cache blueboxes-cache?) 
                             (or/c #f (non-empty-listof string?)))]
  [make-blueboxes-cache (->* (boolean?) (#:blueboxes-dirs (listof path?)) blueboxes-cache?)]
  [blueboxes-cache? (-> any/c boolean?)]))

(struct blueboxes-cache (info-or-paths) #:mutable)
(define (make-blueboxes-cache
         populate?
         #:blueboxes-dirs
         [blueboxes-dirs (for*/list ([d (in-list (get-doc-search-dirs))]
                                     [c (in-list (if (directory-exists? d)
                                                     (directory-list d)
                                                     '()))])
                           (build-path d c))])
  (define cache (blueboxes-cache blueboxes-dirs))
  (when populate? (populate-cache! cache))
  cache)

(define (fetch-blueboxes-strs tag #:blueboxes-cache [cache (make-blueboxes-cache #f)])
  (define plain-strs (fetch-strs-for-single-tag tag cache))
  (cond
    [(and plain-strs
          (pair? tag)
          (eq? (car tag) 'def))
     (define constructor-strs 
       (fetch-strs-for-single-tag (cons 'constructor (cdr tag)) cache))
     (if constructor-strs
         (append plain-strs 
                 '("") 
                 ;; cdr drops the "white label" line (constructor, presumably)
                 (cdr constructor-strs))
         plain-strs)]
    [else
     plain-strs]))

(define (fetch-strs-for-single-tag tag cache)
  (populate-cache! cache)
  (for/or ([ent (in-list (blueboxes-cache-info-or-paths cache))])
    (define offset+lens (hash-ref (list-ref ent 2) tag #f))
    (cond
      [offset+lens
       (apply
        append
        (for/list ([offset+len (in-list offset+lens)])
          (define fn (list-ref ent 0))
          (define offset (list-ref ent 1))
          (call-with-input-file fn
            (λ (port)
              (port-count-lines! port)
              (file-position port (+ (car offset+len) offset))
              (for/list ([i (in-range (cdr offset+len))])
                (read-line port))))))]
      [else #f])))

(define (populate-cache! cache)
  (define cache-content (blueboxes-cache-info-or-paths cache))
  (when ((listof path?) cache-content)
    (set-blueboxes-cache-info-or-paths! cache (build-blueboxes-cache cache-content))))

;; build-blueboxes-cache : (listof (list file-path int hash[tag -o> (cons int int)]))
(define (build-blueboxes-cache blueboxes-dirs)
  (filter
   values
   (for*/list ([doc-dir-name (in-list blueboxes-dirs)])
     (define blueboxes.rktd (build-path doc-dir-name "blueboxes.rktd"))
     (and (file-exists? blueboxes.rktd)
          (call-with-input-file blueboxes.rktd
            (λ (port)
              (port-count-lines! port)
              (define first-line (read-line port))
              (define pos (file-position port))
              (define desed 
                (with-handlers ([exn:fail? (λ (x) 
                                             (log-warning "Failed to deserialize ~a: ~a"
                                                          x
                                                          (exn-message x))
                                             #f)])
                  (deserialize (read port))))
              (and desed
                   (list blueboxes.rktd
                         (+ (string->number first-line) pos)
                         desed))))))))

#lang racket/base
(require setup/dirs
         racket/serialize
         racket/contract
         racket/match
         scribble/core
         scribble/tag)

(provide
 (contract-out
  [fetch-blueboxes-strs (->* (tag?) (#:blueboxes-cache blueboxes-cache?) 
                             (or/c #f (non-empty-listof string?)))]
  [make-blueboxes-cache (->* (boolean?) (#:blueboxes-dirs (listof path?)) blueboxes-cache?)]
  [blueboxes-cache? (-> any/c boolean?)]
  [fetch-blueboxes-method-tags (->* (symbol?) (#:blueboxes-cache blueboxes-cache?)
                                    (listof method-tag?))]))

(struct blueboxes-cache (info-or-paths method->tags) #:mutable)
(define (make-blueboxes-cache
         populate?
         #:blueboxes-dirs
         [blueboxes-dirs (for*/list ([d (in-list (get-doc-search-dirs))]
                                     [c (in-list (if (directory-exists? d)
                                                     (directory-list d)
                                                     '()))])
                           (build-path d c))])
  (define cache (blueboxes-cache blueboxes-dirs #f))
  (when populate? (populate-cache! cache))
  cache)

(define (fetch-blueboxes-strs tag #:blueboxes-cache [cache (make-blueboxes-cache #f)])
  (define plain-strs (fetch-strs-for-single-tag tag cache))
  (cond
    [(and plain-strs (definition-tag? tag))
     (define constructor-strs 
       (fetch-strs-for-single-tag
        (class/interface-tag->constructor-tag
         (definition-tag->class/interface-tag tag))
        cache))
     (append plain-strs
             (if constructor-strs '("") '())
             (if constructor-strs (cdr constructor-strs) '()))]
    [else plain-strs]))

(define (fetch-strs-for-single-tag tag cache)
  (populate-cache! cache)
  (for/or ([ent (in-list (blueboxes-cache-info-or-paths cache))])
    (define offset+lens (hash-ref (list-ref ent 2) tag #f))
    (cond
      [offset+lens
       (define lines
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
                  (read-line port)))))))
       (cond
         [(ormap eof-object? lines) #f]
         [else lines])]
      [else #f])))

(define (fetch-blueboxes-method-tags sym #:blueboxes-cache [cache (make-blueboxes-cache #f)])
  (populate-cache! cache)
  (hash-ref (blueboxes-cache-method->tags cache) sym '()))

(define (populate-cache! cache)
  (define cache-content (blueboxes-cache-info-or-paths cache))
  (when ((listof path?) cache-content)
    (set-blueboxes-cache-info-or-paths! cache (build-blueboxes-cache cache-content))
    (define mtd-table (compute-methods-table (blueboxes-cache-info-or-paths cache)))
    (set-blueboxes-cache-method->tags! cache mtd-table)))

(define (compute-methods-table lst)
  (define meth-ht (make-hash))
  (for ([three-tuple (in-list lst)])
    (match three-tuple
      [`(,file-path ,i ,tag-ht)
       (for ([(tag val) (in-hash tag-ht)])
         (when (method-tag? tag)
           (define-values (class/intf meth) (get-class/interface-and-method tag))
           (hash-set! meth-ht meth (cons tag (hash-ref meth-ht meth '())))))]))
  meth-ht)

;; build-blueboxes-cache : ... -> (listof (list file-path int valid-blueboxes-info?))
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
                  (define candidate (deserialize (read port)))
                  (unless (valid-blueboxes-info? candidate)
                    (error 'build-blueboxes-cache
                           "blueboxes info didn't have the right shape: ~s"
                           candidate))
                  candidate))
              (and desed
                   (list blueboxes.rktd
                         (+ (string->number first-line) pos)
                         desed))))))))


(define valid-blueboxes-info?
  (hash/c
   tag?
   (listof (cons/dc [hd exact-nonnegative-integer?]
                    [tl (hd) (and/c exact-nonnegative-integer?
                                    (>/c hd))]
                    #:flat))
   #:flat? #t))

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


;; offset : (or/c exact-nonnegative-integer? #f)
;; tag-ht : (or/c valid-blueboxes-info? #f)
;; mod-time : (or/c exact-nonnegative-integer? #f)
(struct bluebox-info (blueboxes.rktd [offset #:mutable] [tag-ht #:mutable] [mod-time #:mutable]))

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
    (check-and-update-bluebox-info! ent)
    (match ent
      [(bluebox-info blueboxes.rktd offset tag-ht _)
       (define offset+lens (and tag-ht (hash-ref tag-ht tag #f)))
       (cond
         [offset+lens
          (define lines
            (apply
             append
             (for/list ([offset+len (in-list offset+lens)])
               (call-with-input-file blueboxes.rktd
                 (λ (port)
                   (port-count-lines! port)
                   (file-position port (+ (car offset+len) offset))
                   (for/list ([i (in-range (cdr offset+len))])
                     (read-line port)))))))
          (cond
            [(ormap eof-object? lines) #f]
            [else lines])]
         [else #f])])))

(define (fetch-blueboxes-method-tags sym #:blueboxes-cache [cache (make-blueboxes-cache #f)])
  (populate-cache! cache)
  (hash-ref (blueboxes-cache-method->tags cache) sym '()))

(define (populate-cache! cache)
  (define cache-content (blueboxes-cache-info-or-paths cache))
  (when ((listof path?) cache-content)
    (define the-cache (build-blueboxes-cache cache-content))
    (define mtd-table (compute-methods-table the-cache))
    (set-blueboxes-cache-method->tags! cache mtd-table)
    (set-blueboxes-cache-info-or-paths! cache the-cache)))

(define (compute-methods-table lst)
  (define meth-ht (make-hash))
  (for ([a-bluebox-info (in-list lst)])
    (match a-bluebox-info
      [(bluebox-info blueboxes.rktd offset tag-ht mod-time)
       (when tag-ht
         (for ([(tag val) (in-hash tag-ht)])
           (when (method-tag? tag)
             (define-values (class/intf meth) (get-class/interface-and-method tag))
             (hash-set! meth-ht meth (cons tag (hash-ref meth-ht meth '()))))))]))
  meth-ht)

;; build-blueboxes-cache : ... -> (listof (list file-path int valid-blueboxes-info?))
(define (build-blueboxes-cache blueboxes-dirs)
  (filter
   values
   (for*/list ([doc-dir-name (in-list blueboxes-dirs)])
     (define blueboxes.rktd (build-path doc-dir-name "blueboxes.rktd"))
     (define a-bluebox-info (bluebox-info blueboxes.rktd #f #f #f))
     (populate-bluebox-info! a-bluebox-info)
     a-bluebox-info)))

(define (check-and-update-bluebox-info! a-bluebox-info)
  (match a-bluebox-info
    [(bluebox-info blueboxes.rktd offset tag-ht mod-time)
     (when (or (not mod-time)
               (and (file-exists? blueboxes.rktd)
                    (not (mod-time . = . (file-or-directory-modify-seconds blueboxes.rktd)))))
       (populate-bluebox-info! a-bluebox-info))]))

(define (populate-bluebox-info! a-bluebox-info)
  (define blueboxes.rktd (bluebox-info-blueboxes.rktd a-bluebox-info))
  (cond
    [(file-exists? blueboxes.rktd)
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
         (set-bluebox-info-offset! a-bluebox-info (and desed (+ (string->number first-line) pos)))
         (set-bluebox-info-tag-ht! a-bluebox-info desed)
         (set-bluebox-info-mod-time! a-bluebox-info
                                     (file-or-directory-modify-seconds blueboxes.rktd))))]
    [else
     (set-bluebox-info-offset! a-bluebox-info #f)
     (set-bluebox-info-tag-ht! a-bluebox-info #f)
     (set-bluebox-info-mod-time! a-bluebox-info #f)]))

(define valid-blueboxes-info?
  (hash/c
   tag?
   (listof (cons/dc [hd exact-nonnegative-integer?]
                    [tl (hd) (and/c exact-nonnegative-integer?
                                    (>/c hd))]
                    #:flat))
   #:flat? #t))

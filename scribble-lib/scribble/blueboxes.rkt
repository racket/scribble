#lang typed/racket/base
(require racket/match)
(require/typed setup/dirs [get-doc-search-dirs (-> (Listof Path))])
(require/typed racket/serialize [deserialize (Any -> Any)])
(require/typed scribble/core [#:opaque Tag tag?])
(require/typed scribble/tag
               [#:opaque Method-Tag method-tag?]
               [#:opaque Definition-Tag definition-tag?]
               [#:opaque Class/Interface-Tag class/interface-tag?]
               [class/interface-tag->constructor-tag (Class/Interface-Tag -> Tag)]
               [definition-tag->class/interface-tag (Definition-Tag -> Class/Interface-Tag)]
               [get-class/interface-and-method (Method-Tag -> (values Symbol Symbol))]
               )
(require/typed "valid-blueboxes-info.rkt" [valid-blueboxes-info? (Any -> Boolean)])

(provide fetch-blueboxes-strs
         make-blueboxes-cache
         blueboxes-cache?
         fetch-blueboxes-method-tags
         )

(define-type Bluebox-Info bluebox-info)
(struct bluebox-info
  ([blueboxes.rktd : Path-String]
   [offset : (U Natural #f)]
   [tag-ht : (U Blueboxes-Info-Hash #f)] ; (or/c valid-blueboxes-info? #f)
   [mod-time : (U Natural #f)])
  #:mutable)

(define-type Blueboxes-Cache blueboxes-cache)
(struct blueboxes-cache
  ([info-or-paths : (U (Listof Path) (Listof Bluebox-Info))]
   [method->tags : (U (HashTable Symbol (Listof Method-Tag)) #f)])
  #:mutable)

(: make-blueboxes-cache :
   Boolean
   [#:blueboxes-dirs (Listof Path)]
   ->
   Blueboxes-Cache)
(define (make-blueboxes-cache
         populate?
         #:blueboxes-dirs
         [blueboxes-dirs (for*/list ([d (in-list (get-doc-search-dirs))]
                                     [c (in-list (if (directory-exists? d)
                                                     (directory-list d)
                                                     '()))])
                           : (Listof Path)
                           (build-path d c))])
  (define cache (blueboxes-cache blueboxes-dirs #f))
  (when populate? (populate-cache! cache))
  cache)

(: fetch-blueboxes-strs :
   Tag
   [#:blueboxes-cache Blueboxes-Cache]
   ->
   (U #f (List* String (Listof String))))
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

(: fetch-strs-for-single-tag : Tag Blueboxes-Cache -> (U #f (List* String (Listof String))))
(define (fetch-strs-for-single-tag tag cache)
  (populate-cache! cache)
  (for/or ([ent (in-list (blueboxes-cache-info-or-paths cache))])
    : (U #f (List* String (Listof String)))
    (when (bluebox-info? ent)
      (check-and-update-bluebox-info! ent))
    (match ent
      [(bluebox-info blueboxes.rktd offset tag-ht _)
       (define offset+lens (and tag-ht (hash-ref tag-ht tag #f)))
       (cond
         [offset+lens
          (define lines
            (apply
             append
             (for/list ([offset+len (in-list offset+lens)])
               : (Listof (Listof (U String EOF)))
               (call-with-input-file blueboxes.rktd
                 (λ ([port : Input-Port])
                   (port-count-lines! port)
                   (file-position port (+ (car offset+len) (or offset 0)))
                   (for/list ([i (in-range (cdr offset+len))])
                     : (Listof (U String EOF))
                     (read-line port)))))))
          (cond
            [(not (andmap string? lines)) #f]
            [(null? lines) #f]
            [else lines])]
         [else #f])]
      [_ (log-warning "expected bluebox-info?, given: ~v" ent)
         #f])))

(: fetch-blueboxes-method-tags : Symbol [#:blueboxes-cache Blueboxes-Cache] -> (Listof Method-Tag))
(define (fetch-blueboxes-method-tags sym #:blueboxes-cache [cache (make-blueboxes-cache #f)])
  (populate-cache! cache)
  (define ht (blueboxes-cache-method->tags cache))
  (or (and ht (hash-ref ht sym (λ () '()))) '()))

(define listof-path? (make-predicate (Listof Path)))

(: populate-cache! : Blueboxes-Cache -> Void)
(define (populate-cache! cache)
  (define cache-content (blueboxes-cache-info-or-paths cache))
  (when (listof-path? cache-content)
    (define the-cache (build-blueboxes-cache cache-content))
    (define mtd-table (compute-methods-table the-cache))
    (set-blueboxes-cache-method->tags! cache mtd-table)
    (set-blueboxes-cache-info-or-paths! cache the-cache)))

(: compute-methods-table : (Listof Bluebox-Info) -> (HashTable Symbol (Listof Method-Tag)))
(define (compute-methods-table lst)
  (: meth-ht : (HashTable Symbol (Listof Method-Tag)))
  (define meth-ht (make-hash))
  (for ([a-bluebox-info (in-list lst)])
    (match a-bluebox-info
      [(bluebox-info blueboxes.rktd offset tag-ht mod-time)
       (when tag-ht
         (for ([(tag val) (in-hash tag-ht)])
           (when (method-tag? tag)
             (define-values (class/intf meth) (get-class/interface-and-method tag))
             (hash-set! meth-ht meth (cons tag (hash-ref meth-ht meth (λ () '())))))))]))
  meth-ht)

(: build-blueboxes-cache : (Listof Path) -> (Listof Bluebox-Info))
(define (build-blueboxes-cache blueboxes-dirs)
  (filter
   values
   (for*/list ([doc-dir-name (in-list blueboxes-dirs)])
     : (Listof Bluebox-Info)
     (define blueboxes.rktd (build-path doc-dir-name "blueboxes.rktd"))
     (define a-bluebox-info (bluebox-info blueboxes.rktd #f #f #f))
     (populate-bluebox-info! a-bluebox-info)
     a-bluebox-info)))

(: check-and-update-bluebox-info! : bluebox-info -> Void)
(define (check-and-update-bluebox-info! a-bluebox-info)
  (match a-bluebox-info
    [(bluebox-info blueboxes.rktd offset tag-ht mod-time)
     (when (or (not mod-time)
               (and (file-exists? blueboxes.rktd)
                    (not (mod-time . = . (file-or-directory-modify-seconds blueboxes.rktd)))))
       (populate-bluebox-info! a-bluebox-info))]))

(: populate-bluebox-info! : Bluebox-Info -> Void)
(define (populate-bluebox-info! a-bluebox-info)
  (define blueboxes.rktd (bluebox-info-blueboxes.rktd a-bluebox-info))
  (cond
    [(file-exists? blueboxes.rktd)
     (call-with-input-file blueboxes.rktd
       (λ ([port : Input-Port])
         (port-count-lines! port)
         (define first-line (read-line port))
         (define pos (file-position port))
         (define desed 
           (with-handlers ([exn:fail? (λ ([x : exn:fail])
                                        (log-warning "Failed to deserialize ~a: ~a"
                                                     x
                                                     (exn-message x))
                                        #f)])
             (define candidate (deserialize (read port)))
             (unless (valid-blueboxes-info? candidate)
               (error 'build-blueboxes-cache
                      "blueboxes info didn't have the right shape: ~s"
                      candidate))
             (cast candidate Blueboxes-Info-Hash)))
         (define first-line-num (and (string? first-line) (string->number first-line)))
         (cond
           [(exact-nonnegative-integer? first-line-num)
            (set-bluebox-info-offset! a-bluebox-info (+ first-line-num pos))]
           [else
            (log-warning "expected a string representing a Natuaral\n  given: ~v"
                         first-line-num)])
         (set-bluebox-info-tag-ht! a-bluebox-info desed)
         (set-bluebox-info-mod-time! a-bluebox-info
                                     (file-or-directory-modify-seconds blueboxes.rktd))))]
    [else
     (set-bluebox-info-offset! a-bluebox-info #f)
     (set-bluebox-info-tag-ht! a-bluebox-info #f)
     (set-bluebox-info-mod-time! a-bluebox-info #f)]))

(define-type Blueboxes-Info-Hash
  (HashTable
   Tag
   (Listof (Pairof Natural
                   Natural))))


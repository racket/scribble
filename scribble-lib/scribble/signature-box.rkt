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
(require/typed "valid-signature-box-info.rkt" [valid-signature-box-info? (Any -> Boolean)])

(provide fetch-signature-box-strs
         make-signature-box-cache
         signature-box-cache?
         fetch-signature-box-method-tags
         )

(define-type Bluebox-Info bluebox-info)
(struct bluebox-info
  ([signature-box.rktd : Path-String]
   [offset : (U Natural #f)]
   [tag-ht : (U Signature-box-Info-Hash #f)] ; (or/c valid-signature-box-info? #f)
   [mod-time : (U Natural #f)])
  #:mutable)

(define-type Signature-box-Cache signature-box-cache)
(struct signature-box-cache
  ([info-or-paths : (U (Listof Path) (Listof Bluebox-Info))]
   [method->tags : (U (HashTable Symbol (Listof Method-Tag)) #f)])
  #:mutable)

(: make-signature-box-cache :
   Boolean
   [#:signature-box-dirs (Listof Path)]
   ->
   Signature-box-Cache)
(define (make-signature-box-cache
         populate?
         #:signature-box-dirs
         [signature-box-dirs (for*/list ([d (in-list (get-doc-search-dirs))]
                                     [c (in-list (if (directory-exists? d)
                                                     (directory-list d)
                                                     '()))])
                           : (Listof Path)
                           (build-path d c))])
  (define cache (signature-box-cache signature-box-dirs #f))
  (when populate? (populate-cache! cache))
  cache)

(: fetch-signature-box-strs :
   Tag
   [#:signature-box-cache Signature-box-Cache]
   ->
   (U #f (List* String (Listof String))))
(define (fetch-signature-box-strs tag #:signature-box-cache [cache (make-signature-box-cache #f)])
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

(: fetch-strs-for-single-tag : Tag Signature-box-Cache -> (U #f (List* String (Listof String))))
(define (fetch-strs-for-single-tag tag cache)
  (populate-cache! cache)
  (for/or ([ent (in-list (signature-box-cache-info-or-paths cache))])
    : (U #f (List* String (Listof String)))
    (when (bluebox-info? ent)
      (check-and-update-bluebox-info! ent))
    (match ent
      [(bluebox-info signature-box.rktd offset tag-ht _)
       (define offset+lens (and tag-ht (hash-ref tag-ht tag #f)))
       (cond
         [offset+lens
          (define lines
            (apply
             append
             (for/list ([offset+len (in-list offset+lens)])
               : (Listof (Listof (U String EOF)))
               (call-with-input-file signature-box.rktd
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

(: fetch-signature-box-method-tags : Symbol [#:signature-box-cache Signature-box-Cache] -> (Listof Method-Tag))
(define (fetch-signature-box-method-tags sym #:signature-box-cache [cache (make-signature-box-cache #f)])
  (populate-cache! cache)
  (define ht (signature-box-cache-method->tags cache))
  (or (and ht (hash-ref ht sym (λ () '()))) '()))

(define listof-path? (make-predicate (Listof Path)))

(: populate-cache! : Signature-box-Cache -> Void)
(define (populate-cache! cache)
  (define cache-content (signature-box-cache-info-or-paths cache))
  (when (listof-path? cache-content)
    (define the-cache (build-signature-box-cache cache-content))
    (define mtd-table (compute-methods-table the-cache))
    (set-signature-box-cache-method->tags! cache mtd-table)
    (set-signature-box-cache-info-or-paths! cache the-cache)))

(: compute-methods-table : (Listof Bluebox-Info) -> (HashTable Symbol (Listof Method-Tag)))
(define (compute-methods-table lst)
  (: meth-ht : (HashTable Symbol (Listof Method-Tag)))
  (define meth-ht (make-hash))
  (for ([a-bluebox-info (in-list lst)])
    (match a-bluebox-info
      [(bluebox-info signature-box.rktd offset tag-ht mod-time)
       (when tag-ht
         (for ([(tag val) (in-hash tag-ht)])
           (when (method-tag? tag)
             (define-values (class/intf meth) (get-class/interface-and-method tag))
             (hash-set! meth-ht meth (cons tag (hash-ref meth-ht meth (λ () '())))))))]))
  meth-ht)

(: build-signature-box-cache : (Listof Path) -> (Listof Bluebox-Info))
(define (build-signature-box-cache signature-box-dirs)
  (filter
   values
   (for*/list ([doc-dir-name (in-list signature-box-dirs)])
     : (Listof Bluebox-Info)
     (define signature-box.rktd (build-path doc-dir-name "signature-box.rktd"))
     (define a-bluebox-info (bluebox-info signature-box.rktd #f #f #f))
     (populate-bluebox-info! a-bluebox-info)
     a-bluebox-info)))

(: check-and-update-bluebox-info! : bluebox-info -> Void)
(define (check-and-update-bluebox-info! a-bluebox-info)
  (match a-bluebox-info
    [(bluebox-info signature-box.rktd offset tag-ht mod-time)
     (when (or (not mod-time)
               (and (file-exists? signature-box.rktd)
                    (not (mod-time . = . (file-or-directory-modify-seconds signature-box.rktd)))))
       (populate-bluebox-info! a-bluebox-info))]))

(: populate-bluebox-info! : Bluebox-Info -> Void)
(define (populate-bluebox-info! a-bluebox-info)
  (define signature-box.rktd (bluebox-info-signature-box.rktd a-bluebox-info))
  (cond
    [(file-exists? signature-box.rktd)
     (call-with-input-file signature-box.rktd
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
             (unless (valid-signature-box-info? candidate)
               (error 'build-signature-box-cache
                      "signature-box info didn't have the right shape: ~s"
                      candidate))
             (cast candidate Signature-box-Info-Hash)))
         (define first-line-num (and (string? first-line) (string->number first-line)))
         (cond
           [(exact-nonnegative-integer? first-line-num)
            (set-bluebox-info-offset! a-bluebox-info (+ first-line-num pos))]
           [else
            (log-warning "expected a string representing a Natuaral\n  given: ~v"
                         first-line-num)])
         (set-bluebox-info-tag-ht! a-bluebox-info desed)
         (set-bluebox-info-mod-time! a-bluebox-info
                                     (file-or-directory-modify-seconds signature-box.rktd))))]
    [else
     (set-bluebox-info-offset! a-bluebox-info #f)
     (set-bluebox-info-tag-ht! a-bluebox-info #f)
     (set-bluebox-info-mod-time! a-bluebox-info #f)]))

(define-type Signature-box-Info-Hash
  (HashTable
   Tag
   (Listof (Pairof Natural
                   Natural))))


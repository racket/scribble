#lang racket/base

(require "core.rkt"
         "basic.rkt"
         "search.rkt"
         "private/manual-sprop.rkt"
         "private/on-demand.rkt"
         "html-properties.rkt"
         file/convertible
         racket/extflonum
         (for-syntax racket/base))
  
(provide define-code
         to-element
         to-element/no-color
         to-paragraph
         to-paragraph/prefix
         syntax-ize
         syntax-ize-hook
         current-keyword-list
         current-variable-list
         current-meta-list

         input-color
         output-color
         input-background-color
         no-color
         reader-color
         result-color
         keyword-color
         comment-color
         paren-color
         meta-color
         value-color
         symbol-color
         variable-color
         opt-color
         error-color
         syntax-link-color
         value-link-color
         syntax-def-color
         value-def-color
         module-color
         module-link-color
         block-color
         highlighted-color

         (struct-out var-id)
         (struct-out shaped-parens)
         (struct-out long-boolean)
         (struct-out just-context)
         (struct-out alternate-display)
         (struct-out literal-syntax)
         (for-syntax make-variable-id
                     variable-id?
                     make-element-id-transformer
                     element-id-transformer?))

(module+ id-element
  (provide make-id-element))

(define (make-racket-style s 
                           #:tt? [tt? #t]
                           #:extras [extras null])
  (make-style s (if tt?
                    (cons 'tt-chars 
                          (append extras
                                  scheme-properties))
                    (append extras
                            scheme-properties))))

(define-on-demand output-color (make-racket-style "RktOut"))
(define-on-demand input-color (make-racket-style "RktIn"))
(define-on-demand input-background-color (make-racket-style "RktInBG"))
(define-on-demand no-color (make-racket-style "RktPlain"))
(define-on-demand reader-color (make-racket-style "RktRdr"))
(define-on-demand result-color (make-racket-style "RktRes"))
(define-on-demand keyword-color (make-racket-style "RktKw"))
(define-on-demand comment-color (make-racket-style "RktCmt"))
(define-on-demand paren-color (make-racket-style "RktPn"))
(define-on-demand meta-color (make-racket-style "RktMeta"))
(define-on-demand value-color (make-racket-style "RktVal"))
(define-on-demand symbol-color (make-racket-style "RktSym"))
(define-on-demand symbol-def-color (make-racket-style "RktSymDef"
                                                      #:extras (list (attributes '((class . "RktSym"))))))
(define-on-demand variable-color (make-racket-style "RktVar"))
(define-on-demand opt-color (make-racket-style "RktOpt"))
(define-on-demand error-color (make-racket-style "RktErr" #:tt? #f))
(define-on-demand syntax-link-color (make-racket-style "RktStxLink"))
(define-on-demand value-link-color (make-racket-style "RktValLink"))
(define-on-demand syntax-def-color (make-racket-style "RktStxDef"
                                                      #:extras (list (attributes '((class . "RktStxLink"))))))
(define-on-demand value-def-color (make-racket-style "RktValDef"
                                                     #:extras (list (attributes '((class . "RktValLink"))))))
(define-on-demand module-color (make-racket-style "RktMod"))
(define-on-demand module-link-color (make-racket-style "RktModLink"))
(define-on-demand block-color (make-racket-style "RktBlk"))
(define-on-demand highlighted-color (make-racket-style "highlighted" #:tt? #f))

(define current-keyword-list 
  (make-parameter null))
(define current-variable-list 
  (make-parameter null))
(define current-meta-list 
  (make-parameter null))

(define-struct (sized-element element) (length))

(define-struct (spaces element) (cnt))

;; We really don't want leading hypens (or minus signs) to
;; create a line break after the hyphen. For interior hyphens,
;; line breaking is usually fine.
(define (nonbreak-leading-hyphens s)
  (define m (regexp-match-positions #rx"^-+" s))
  (if m
      (cond
        [(= (cdar m) (string-length s)) (make-element 'no-break s)]
        [else
         (define len (add1 (cdar m)))
         (make-element #f (list (make-element 'no-break (substring s 0 len))
                                (substring s len)))])
      s))

(define (literalize-spaces i [leading? #f])
  (define m (regexp-match-positions #rx"  +" i))
  (cond
    [m
     (define cnt (- (cdar m) (caar m)))
     (make-spaces #f
                  (list
                   (literalize-spaces (substring i 0 (caar m)) #t)
                   (hspace cnt)
                   (literalize-spaces (substring i (cdar m))))
                  cnt)]
    [leading? (nonbreak-leading-hyphens i)]
    [else i]))


(define line-breakable-space (make-element 'tt " "))

;; These caches intentionally record a key with the value.
;; That way, when the value is no longer used, the key
;; goes away, and the entry is gone.

(define id-element-cache (make-weak-hash))
(define element-cache (make-weak-hash))

(define-struct (cached-delayed-element delayed-element) (cache-key))
(define-struct (cached-element element) (cache-key))

(define qq-ellipses (string->uninterned-symbol "..."))

;; Extract/record a value from a weak hash table mapping to weak boxes
;; proc :: (-> (not/c #f))
(define (hash-weak-ref! ht key proc)
  ;; unwrap-weak-box :: (-> (or/c #f weak-box?) (or/c #f (not/c #f)))
  ;; It returns #f when either the weak box is gone,
  ;; or there's no key-value association
  (define (unwrap-weak-box v)
    (cond
      [v (weak-box-value v)]
      [else #f]))
  (cond
    [(unwrap-weak-box (hash-ref ht key #f))]
    [else
     ;; bind to v first so that the GC can't collect it
     (define v (proc))
     (hash-set! ht key (make-weak-box v))
     v]))

(define (make-id-element c s defn?
                         #:space [space #f]
                         #:suffix [suffix space]
                         #:unlinked-ok? [unlinked-ok? #f])
  (define b (identifier-label-binding c))
  (unless b (error 'make-id-element "no for-label binding for identifier: ~s" c))
  (define key
    (vector (syntax-e c)
            (module-path-index->taglet (caddr b))
            (cadddr b)
            (list-ref b 5)
            (syntax-property c 'display-string)
            defn?
            suffix
            s))
  (define (do-make-cached-delayed-element)
    (make-cached-delayed-element
     (lambda (renderer sec ri)
       (define tag (find-racket-tag sec ri c #f
                                    #:space space
                                    #:suffix suffix
                                    #:unlinked-ok? unlinked-ok?))
       (cond
         [tag
          (let ([tag (intern-taglet tag)])
            (list
             (case (car tag)
               [(form)
                (make-link-element (if defn?
                                       syntax-def-color
                                       syntax-link-color)
                                   (nonbreak-leading-hyphens s)
                                   tag)]
               [else
                (make-link-element (if defn?
                                       value-def-color
                                       value-link-color)
                                   (nonbreak-leading-hyphens s)
                                   tag)])))]
         [unlinked-ok?
          (list (make-element symbol-color s))]
         [else
          (list
           (make-element "badlink"
                         (make-element value-link-color s)))]))
     (lambda () s)
     (lambda () s)
     (intern-taglet key)))
  (hash-weak-ref! id-element-cache key do-make-cached-delayed-element))

(define (make-element/cache style content)
  (cond
    [(string? content)
     (define key (vector style content))
     (define (do-make-cached-element)
       (make-cached-element style content key))
     (hash-weak-ref! element-cache key do-make-cached-element)]
    [else (make-element style content)]))

(define (to-quoted obj expr? quote-depth out color? inc!)
  (cond
    [(and expr?
          (zero? quote-depth)
          (quotable? obj))
     (out "'" (and color? value-color))
     (inc!)
     (add1 quote-depth)]
    [else quote-depth]))

(define (to-unquoted expr? quote-depth out color? inc!)
  (cond
    [(or (not expr?) (zero? quote-depth)) quote-depth]
    [else
     (out "," (and color? meta-color))
     (inc!)
     (to-unquoted expr? (sub1 quote-depth) out color? inc!)]))

(define iformat
  (case-lambda
    [(str val) (datum-intern-literal (format str val))]
    [(str . vals) (datum-intern-literal (apply format str vals))]))

(define (typeset-atom c out color? quote-depth expr? escapes? defn?)
  (cond
    [(and (var-id? (syntax-e c)) (zero? quote-depth))
     (define v (var-id-sym (syntax-e c)))
     (out (iformat "~s" (if (syntax? v) (syntax-e v) v))
          variable-color)]
    [else
     (define is-var? (and (identifier? c) (memq (syntax-e c) (current-variable-list))))
     (define sc (syntax-e c))
     (define s-tentative
       (cond
         [(syntax-property c 'display-string) => values]
         [(literal-syntax? sc) (iformat "~s" (literal-syntax-stx sc))]
         [(var-id? sc) (iformat "~s" (var-id-sym sc))]
         [(eq? sc #t)
          (if (equal? (syntax-span c) 5)
              "#true"
              "#t")]
         [(eq? sc #f)
          (if (equal? (syntax-span c) 6)
              "#false"
              "#f")]
         [(and (number? sc)
               (inexact? sc))
          (define s (iformat "~s" sc))
          (if (= (string-length s)
                 (- (syntax-span c) 2))
              ;; There's no way to know whether the source used #i,
              ;; but it should be ok to include it:
              (string-append "#i" s)
              s)]
         [else (iformat "~s" sc)]))
     (define-values (s it?)
       (if (and escapes?
                (symbol? sc)
                ((string-length s-tentative) . > . 1)
                (char=? (string-ref s-tentative 0) #\_)
                (not (or (identifier-label-binding c) is-var?)))
           (values (substring s-tentative 1) #t)
           (values s-tentative #f)))
     (let ([quote-depth (if (and expr? (identifier? c) (not (eq? qq-ellipses (syntax-e c))))
                            (let ([quote-depth
                                   (if (and (quote-depth . < . 2)
                                            (memq (syntax-e c) '(unquote unquote-splicing)))
                                       (to-unquoted expr? quote-depth out color? void)
                                       quote-depth)])
                              (to-quoted c expr? quote-depth out color? void))
                            quote-depth)])
       (if (or (element? (syntax-e c))
               (delayed-element? (syntax-e c))
               (part-relative-element? (syntax-e c))
               (convertible? (syntax-e c)))
           (out (syntax-e c) #f)
           (out (if (and (identifier? c)
                         color?
                         (quote-depth . <= . 0)
                         (not (or it? is-var?)))
                    (cond
                      [(pair? (identifier-label-binding c))
                       (make-id-element c s defn?)]
                      [else
                       (define c (nonbreak-leading-hyphens s))
                       (if defn?
                           (make-element symbol-def-color c)
                           c)])
                    (literalize-spaces s #t))
                (cond
                  [(positive? quote-depth) value-color]
                  [(let ([v (syntax-e c)])
                     (or (number? v)
                         (string? v)
                         (bytes? v)
                         (char? v)
                         (regexp? v)
                         (byte-regexp? v)
                         (boolean? v)
                         (extflonum? v)))
                   value-color]
                  [(identifier? c)
                   (cond
                     [is-var?
                      variable-color]
                     [(and (identifier? c)
                           (memq (syntax-e c) (current-keyword-list)))
                      keyword-color]
                     [(and (identifier? c)
                           (memq (syntax-e c) (current-meta-list)))
                      meta-color]
                     [it? variable-color]
                     [else symbol-color])]
                  [else paren-color])
                (string-length s))))]))

(define omitable (make-style #f '(omitable)))

(define (gen-typeset c multi-line? prefix1 prefix suffix color? expr? escapes? defn? elem-wrap)
  (let* ([c (syntax-ize c 0 #:expr? expr?)]
         [content null]
         [docs null]
         [first (if escapes?
                    (syntax-case c (code:line)
                      [(code:line e . rest) #'e]
                      [_ c])
                    c)]
         [init-col (or (syntax-column first) 0)]
         [src-col init-col]
         [inc-src-col (lambda () (set! src-col (add1 src-col)))]
         [dest-col 0]
         [highlight? #f]
         [col-map (make-hash)]
         [next-col-map (make-hash)]
         [line (or (syntax-line first) 0)])
    (define (finish-line!)
      (when multi-line?
        (set! docs (cons (make-paragraph omitable 
                                         (if (null? content)
                                             (list (hspace 1))
                                             (reverse content)))
                         docs))
        (set! content null)))
    (define out
      (case-lambda
        [(v cls)
         (out v cls (let sz-loop ([v v])
                      (cond
                        [(string? v) (string-length v)]
                        [(list? v) (for/fold ([s 0]) ([v (in-list v)]) (+ s (sz-loop v)))]
                        [(sized-element? v) (sized-element-length v)]
                        [(element? v)
                         (sz-loop (element-content v))]
                        [(delayed-element? v)
                         (content-width v)]
                        [(part-relative-element? v)
                         (content-width v)]
                        [(spaces? v)
                         (+ (sz-loop (car (element-content v)))
                            (spaces-cnt v)
                            (sz-loop (caddr (element-content v))))]
                        [else 1])))]
        [(v cls len)
         (unless (equal? v "")
           (cond
             [(spaces? v)
              (out (car (element-content v)) cls 0)
              (out (cadr (element-content v)) #f 0)
              (out (caddr (element-content v)) cls len)]
             [(equal? v "\n")
              (cond
                [multi-line?
                 (finish-line!)
                 (out prefix cls)]
                [else (out " " cls)])]
             [else
              (set! content (cons (elem-wrap
                                   ((if highlight?
                                        (lambda (c)
                                          (make-element highlighted-color c))
                                        values)
                                    (if (and color? cls)
                                        (make-element/cache cls v)
                                        v)))
                                  content))
              (set! dest-col (+ dest-col len))]))]))
    (define advance
      (case-lambda
        [(c init-line! srcless-step delta)
         (let ([c (+ delta (or (syntax-column c)
                               (if srcless-step
                                   (+ src-col srcless-step)
                                   0)))]
               [l (syntax-line c)])
           (define new-line? (and l (l . > . line)))
           (when new-line?
             (for ([i (in-range (- l line))])
               (out "\n" #f))
             (set! line l)
             (set! col-map next-col-map)
             (set! next-col-map (make-hash))
             (init-line!))
           (let ([d-col (let ([def-val (+ dest-col (- c src-col))])
                          (if new-line?
                              (hash-ref col-map c def-val)
                              def-val))])
             (define amt (- d-col dest-col))
             (when (positive? amt)
               (define old-dest-col dest-col)
               (out (if (and (= 1 amt) (not multi-line?))
                        line-breakable-space ; allows a line break to replace the space
                        (hspace amt))
                    #f)
               (set! dest-col (+ old-dest-col amt))))
           (set! src-col c)
           (hash-set! next-col-map src-col dest-col))]
        [(c init-line! srcless-step) (advance c init-line! srcless-step 0)]
        [(c init-line!) (advance c init-line! #f 0)]))
    (define (for-each/i f l v)
      (unless (null? l)
        (f (car l) v)
        (for-each/i f (cdr l) 1)))
    (define (convert-infix c quote-depth expr?)
      (define l (syntax->list c))
      (and l
           ((length l) . >= . 3)
           ((or (syntax-position (car l)) -inf.0)
            . > .
            (or (syntax-position (cadr l)) +inf.0))
           (let ([a (car l)])
             (let loop ([l (cdr l)]
                        [prev null])
               (cond
                 [(null? l) #f] ; couldn't unwind
                 [else
                  (define p2 (syntax-position (car l)))
                  (if (and p2
                           (p2 . > . (syntax-position a)))
                      (datum->syntax c
                                     (append
                                      (reverse prev)
                                      (list
                                       (datum->syntax
                                        a
                                        (let ([val? (positive? quote-depth)])
                                          (make-sized-element
                                           (if val? value-color #f)
                                           (list
                                            (make-element/cache (if val? value-color paren-color) '". ")
                                            (typeset a #f "" "" "" (not val?) expr? escapes? defn? elem-wrap)
                                            (make-element/cache (if val? value-color paren-color) '" ."))
                                           (+ (syntax-span a) 4)))
                                        (list (syntax-source a)
                                              (syntax-line a)
                                              (- (syntax-column a) 2)
                                              (- (syntax-position a) 2)
                                              (+ (syntax-span a) 4))
                                        a))
                                      l)
                                     c
                                     c)
                      (loop (cdr l)
                            (cons (car l) prev)))])))))
    (define (no-fancy-chars s)
      (cond
        [(eq? s 'rsquo) "'"]
        [else s]))

    #; {Syntax Init-line! Srcless-step String -> Comment}
    [define (make-comment c init-line! srcless-step comment-string)
      (check-1-c c)
      (advance c init-line! srcless-step)
      (out comment-string comment-color)
      (out 'nbsp comment-color)
      (define v (syntax->datum (cadr (syntax->list c))))
      (if (paragraph? v)
          (map (lambda (v)
                 (let ([v (no-fancy-chars v)])
                   (if (or (string? v) (symbol? v))
                       (out v comment-color)
                       (out v #f))))
               (paragraph-content v))
          (out (no-fancy-chars v) comment-color))]

    #; {Syntax Init-line! Srcless-step String String -> Contract}
    (define (make-contract c init-line! srcless-step start-comment continue-comment)
      (advance c init-line! srcless-step)
      (out start-comment comment-color)
      (define l (cdr (syntax->list c)))
      (define s-col (or (syntax-column (car l)) src-col))
      (set! src-col s-col)
      (for-each/i (loop (lambda ()
                          (set! src-col s-col)
                          (set! dest-col 0)
                          (out continue-comment comment-color))
                        0
                        expr?
                        #f)
                  l
                  #f))

    #; {Syntax -> Void}
    (define (check-1-c c)
      (define l (syntax->list c))
      (unless (and l (= 2 (length l)))
        (raise-syntax-error #f "does not have a single sub-form" c)))

    (define ((loop init-line! quote-depth expr? no-cons?) c srcless-step)
      (cond
        [(and escapes? (eq? 'code:blank (syntax-e c)))
         (advance c init-line! srcless-step)]
        [(and escapes? (pair? (syntax-e c)) (eq? (syntax-e (car (syntax-e c))) 'code:comment))
         (make-comment c init-line! srcless-step  ";")]
        [(and escapes? (pair? (syntax-e c)) (eq? (syntax-e (car (syntax-e c))) 'code:comment2))
         (make-comment c init-line! srcless-step ";;")]
        [(and escapes? (pair? (syntax-e c)) (eq? (syntax-e (car (syntax-e c))) 'code:comment#))
         (make-comment c init-line! srcless-step "#;")]
        [(and escapes? (pair? (syntax-e c)) (eq? (syntax-e (car (syntax-e c))) 'code:contract))
         (make-contract c init-line! srcless-step "; " "; ")]
        [(and escapes? (pair? (syntax-e c)) (eq? (syntax-e (car (syntax-e c))) 'code:contract#))
         (check-1-c c)
         ;; shape is (for eample)
         #; (code:comment# {Natural -> [Listof Natural]})
         (make-contract c init-line! srcless-step "#; " "    ")]
        [(and escapes?
              (pair? (syntax-e c))
              (eq? (syntax-e (car (syntax-e c))) 'code:line))
         (define l (cdr (syntax->list c)))
         (for-each/i (loop init-line! quote-depth expr? #f)
                     l
                     #f)]
        [(and escapes?
              (pair? (syntax-e c))
              (eq? (syntax-e (car (syntax-e c))) 'code:hilite))
         (define l (syntax->list c))
         (define h? highlight?)
         (unless (and l (= 2 (length l)))
           (error "bad code:redex: ~.s" (syntax->datum c)))
         (advance c init-line! srcless-step)
         (set! src-col (syntax-column (cadr l)))
         (hash-set! next-col-map src-col dest-col)
         (set! highlight? #t)
         ((loop init-line! quote-depth expr? #f) (cadr l) #f)
         (set! highlight? h?)
         (set! src-col (add1 src-col))]
        [(and escapes?
              (pair? (syntax-e c))
              (eq? (syntax-e (car (syntax-e c))) 'code:quote))
         (check-1-c c)
         (advance c init-line! srcless-step)
         (let ([quote-depth (to-quoted c expr? quote-depth out color? inc-src-col)])
           (out "(" (if (positive? quote-depth) value-color paren-color))
           (set! src-col (+ src-col 1))
           (hash-set! next-col-map src-col dest-col)
           ((loop init-line! quote-depth expr? #f)
            (datum->syntax #'here 'quote (car (syntax-e c)))
            #f)
           (for-each/i (loop init-line! (add1 quote-depth) expr? #f)
                       (cdr (syntax->list c))
                       1)
           (out ")" (if (positive? quote-depth) value-color paren-color))
           (set! src-col (+ src-col 1))
           #;
           (hash-set! next-col-map src-col dest-col))]
        [(and (pair? (syntax-e c))
              (memq (syntax-e (car (syntax-e c)))
                    '(quote quasiquote unquote unquote-splicing
                            quasisyntax syntax unsyntax unsyntax-splicing))
              (let ([v (syntax->list c)])
                (and v (= 2 (length v))))
              (or (not expr?)
                  (positive? quote-depth)
                  (quotable? c)))
         (advance c init-line! srcless-step)
         (let ([quote-depth (to-quoted c expr? quote-depth out color? inc-src-col)])
           (define-values (str quote-delta)
             (case (syntax-e (car (syntax-e c)))
               [(quote) (values "'" +inf.0)]
               [(unquote) (values "," -1)]
               [(unquote-splicing) (values ",@" -1)]
               [(quasiquote) (values "`" +1)]
               [(syntax) (values "#'" 0)]
               [(quasisyntax) (values "#`" 0)]
               [(unsyntax) (values "#," 0)]
               [(unsyntax-splicing) (values "#,@" 0)]))
           (out str (if (positive? (+ quote-depth quote-delta))
                        value-color
                        reader-color))
           (define i (cadr (syntax->list c)))
           (set! src-col (or (syntax-column i) src-col))
           (hash-set! next-col-map src-col dest-col)
           ((loop init-line! (max 0 (+ quote-depth quote-delta)) expr? #f) i #f))]
        [(and (pair? (syntax-e c))
              (or (not expr?)
                  (positive? quote-depth)
                  (quotable? c))
              (convert-infix c quote-depth expr?))
         => (lambda (converted)
              ((loop init-line! quote-depth expr? #f) converted srcless-step))]
        [(or (pair? (syntax-e c))
             (mpair? (syntax-e c))
             (forced-pair? (syntax-e c))
             (null? (syntax-e c))
             (vector? (syntax-e c))
             (and (struct? (syntax-e c))
                  (prefab-struct-key (syntax-e c)))
             (struct-proxy? (syntax-e c)))
         (let* ([sh (or (syntax-property c 'paren-shape)
                        (if (and (mpair? (syntax-e c))
                                 (not (and expr? (zero? quote-depth))))
                            #\{
                            #\())]
                [quote-depth (if (and (not expr?)
                                      (zero? quote-depth)
                                      (or (vector? (syntax-e c))
                                          (struct? (syntax-e c))))
                                 1
                                 quote-depth)]
                [p-color (if (positive? quote-depth)
                             value-color
                             paren-color)])
           (advance c init-line! srcless-step)
           (let ([quote-depth (if (struct-proxy? (syntax-e c))
                                  quote-depth
                                  (to-quoted c expr? quote-depth out color? inc-src-col))])
             (when (and expr? (zero? quote-depth))
               (out "(" p-color)
               (unless no-cons?
                 (out (let ([s (cond
                                 [(pair? (syntax-e c))
                                  (cond
                                    [(syntax->list c) "list"]
                                    [(let ([d (cdr (syntax-e c))])
                                       (or (pair? d)
                                           (and (syntax? d)
                                                (pair? (syntax-e d)))))
                                     "list*"]
                                    [else "cons"])]
                                 [(vector? (syntax-e c)) "vector"]
                                 [(mpair? (syntax-e c)) "mcons"]
                                 [else (iformat "~a"
                                                (if (struct-proxy? (syntax-e c))
                                                    (syntax-e (struct-proxy-name (syntax-e c)))
                                                    (object-name (syntax-e c))))])])
                        (set! src-col (+ src-col (if (struct-proxy? (syntax-e c))
                                                     1
                                                     (string-length s))))
                        s)
                      symbol-color)
                 (unless (and (struct-proxy? (syntax-e c))
                              (null? (struct-proxy-content (syntax-e c))))
                   (out " " #f))))
             (when (vector? (syntax-e c))
               (unless (and expr? (zero? quote-depth))
                 (define vec (syntax-e c))
                 (out "#" p-color)
                 ;; A vector literal looks like "#(  x   y   z  )".
                 ;; At this point we want to advance src-col past "#".
                 ;; However, after this we will need to advance src-loc to
                 ;; account for "(" and spaces to reach the first element.
                 ;; The "(" component is handled by unconditionally adding 1 below.
                 ;; For simplicity, we will handle the spaces component right now
                 ;; along with advancing src-col past "#",
                 ;; even though it's technically not the right place to do it.
                 (if (zero? (vector-length vec))
                     ;; assume no srcloc means "#()"
                     (set! src-col
                           (+ src-col
                              (- (or (syntax-span c) 3) 2)))
                     ;; assume no srcloc means "#(x ...)";
                     ;; first element appears at the third character
                     (set! src-col
                           (+ src-col
                              (cond
                                [(and (syntax-column (vector-ref vec 0))
                                      (syntax-column c))
                                 (- (syntax-column (vector-ref vec 0))
                                    (syntax-column c)
                                    1)]
                                [else 1]))))))
             (when (struct? (syntax-e c))
               (unless (and expr? (zero? quote-depth))
                 (out "#s" p-color)
                 (set! src-col (+ src-col 2))))
             (unless (and expr? (zero? quote-depth))
               (out (case sh
                      [(#\[) "["]
                      [(#\{) "{"]
                      [else "("])
                    p-color))
             (set! src-col (+ src-col 1))
             (hash-set! next-col-map src-col dest-col)
             (let lloop ([l (cond
                              [(vector? (syntax-e c))
                               (vector->short-list (syntax-e c) syntax-e)]
                              [(struct? (syntax-e c))
                               (define l (vector->list (struct->vector (syntax-e c))))
                               ;; Need to build key datum, syntax-ize it internally, and
                               ;;  set the overall width to fit right:
                               (if (and expr? (zero? quote-depth))
                                   (cdr l)
                                   (cons (let ([key (syntax-ize (prefab-struct-key (syntax-e c))
                                                                (+ 3 (or (syntax-column c) 0))
                                                                (or (syntax-line c) 1))]
                                               [end (if (pair? (cdr l))
                                                        (and (equal? (syntax-line c) (syntax-line (cadr l)))
                                                             (syntax-column (cadr l)))
                                                        (and (syntax-column c)
                                                             (+ (syntax-column c) (syntax-span c))))])
                                           (if end
                                               (datum->syntax #f
                                                              (syntax-e key)
                                                              (vector #f (syntax-line key)
                                                                      (syntax-column key)
                                                                      (syntax-position key)
                                                                      (max 1 (- end 1 (syntax-column key)))))
                                               end))
                                         (cdr l)))]
                              [(struct-proxy? (syntax-e c))
                               (struct-proxy-content (syntax-e c))]
                              [(forced-pair? (syntax-e c))
                               (syntax-e c)]
                              [(mpair? (syntax-e c))
                               (syntax-e c)]
                              [else c])]
                         [first-expr? (and expr?
                                           (or (zero? quote-depth)
                                               (not (struct-proxy? (syntax-e c))))
                                           (not no-cons?))]
                         [dotted? #f]
                         [srcless-step #f])
               (cond
                 [(and (syntax? l)
                       (pair? (syntax-e l))
                       (not dotted?)
                       (not (and (memq (syntax-e (car (syntax-e l)))
                                       '(quote unquote syntax unsyntax quasiquote quasiunsyntax))
                                 (let ([v (syntax->list l)])
                                   (and v (= 2 (length v))))
                                 (or (not expr?)
                                     (quote-depth . > . 1)
                                     (not (memq (syntax-e (car (syntax-e l)))
                                                '(unquote unquote-splicing)))))))
                  (lloop (syntax-e l) first-expr? #f srcless-step)]
                 [(and (or (null? l)
                           (and (syntax? l)
                                (null? (syntax-e l)))))
                  (void)]
                 [(and (pair? l) (not dotted?))
                  ((loop init-line! quote-depth first-expr? #f) (car l) srcless-step)
                  (lloop (cdr l) expr? #f 1)]
                 [(forced-pair? l)
                  ;; forced pairs are for hash tables, where the `car` cannot be
                  ;; unquoted: use +inf.0 for `quote-depth`
                  ((loop init-line! +inf.0 first-expr? #f) (forced-pair-car l) srcless-step)
                  (lloop (forced-pair-cdr l) expr? #t 1)]
                 [(mpair? l)
                  ((loop init-line! quote-depth first-expr? #f) (mcar l) srcless-step)
                  (lloop (mcdr l) expr? #t 1)]
                 [else
                  (unless (and expr? (zero? quote-depth))
                    (advance l init-line! (and srcless-step (+ srcless-step 3)) -2)
                    (out ". " (if (positive? quote-depth) value-color paren-color))
                    (set! src-col (+ src-col 3)))
                  (hash-set! next-col-map src-col dest-col)
                  ((loop init-line! quote-depth first-expr? #f) l (if (and expr? (zero? quote-depth))
                                                                      srcless-step
                                                                      #f))]))
             (out (case sh
                    [(#\[) "]"]
                    [(#\{) "}"]
                    [else ")"])
                  p-color)
             (set! src-col (+ src-col 1))))]
        [(box? (syntax-e c))
         (advance c init-line! srcless-step)
         (let ([quote-depth (to-quoted c expr? quote-depth out color? inc-src-col)])
           (cond
             [(and expr? (zero? quote-depth))
              (out "(" paren-color)
              (out "box" symbol-color)
              (out " " #f)
              (set! src-col (+ src-col 5))]
             [else
              (out "#&" value-color)
              (set! src-col (+ src-col 2))])
           (hash-set! next-col-map src-col dest-col)
           ((loop init-line! (if expr? quote-depth +inf.0) expr? #f) (unbox (syntax-e c)) #f)
           (when (and expr? (zero? quote-depth))
             (out ")" paren-color)))]
        [(hash? (syntax-e c))
         (advance c init-line! srcless-step)
         (define hash-type
           (cond
             [(hash-equal? (syntax-e c)) 'hash]
             [(hash-eqv? (syntax-e c)) 'hasheqv]
             [(hash-eq? (syntax-e c)) 'hasheq]
             [(hash-equal-always? (syntax-e c)) 'hashalw]
             [else (error 'typeset "unexpected hash table type: ~s" (syntax-e c))]))
         (let ([quote-depth (to-quoted c expr? quote-depth out color? inc-src-col)])
           (unless (and expr? (zero? quote-depth))
             (out (iformat "#~a" hash-type) value-color))
           (define delta (+ 1
                            (string-length (symbol->string hash-type))
                            (if (and expr? (zero? quote-depth)) 1 0)))
           (define orig-col src-col)
           (set! src-col (+ src-col delta))
           (hash-set! next-col-map src-col dest-col)
           ((loop init-line! quote-depth expr? (and expr? (zero? quote-depth)))
            (let*-values ([(l) (sort (hash-map (syntax-e c) cons)
                                     (lambda (a b)
                                       (< (or (syntax-position (cdr a)) -inf.0)
                                          (or (syntax-position (cdr b)) -inf.0))))]
                          [(sep cap) (if (and expr? (zero? quote-depth))
                                         (values 1 0)
                                         (values 3 1))]
                          [(col0) (+ (syntax-column c) delta cap 1)]
                          [(l2 pos line) (for/fold ([l2 null][col col0][line (syntax-line c)])
                                                   ([p (in-list l)])
                                           (let* ([tentative (syntax-ize (car p) 0
                                                                         #:expr? (and expr? (zero? quote-depth)))]
                                                  [width (syntax-span tentative)]
                                                  [col (if (= line (syntax-line (cdr p)))
                                                           col
                                                           col0)])
                                             (define key
                                               (let ([e (syntax-ize (car p)
                                                                    (max 0 (- (syntax-column (cdr p))
                                                                              width
                                                                              sep))
                                                                    (syntax-line (cdr p))
                                                                    #:expr? (and expr? (zero? quote-depth)))])
                                                 (if ((syntax-column e) . <= . col)
                                                     e
                                                     (datum->syntax #f
                                                                    (syntax-e e)
                                                                    (vector (syntax-source e)
                                                                            (syntax-line e)
                                                                            col
                                                                            (syntax-position e)
                                                                            (+ (syntax-span e) (- (syntax-column e) col)))))))
                                             (define elem
                                               (datum->syntax
                                                #f
                                                (make-forced-pair key (cdr p))
                                                (vector 'here
                                                        (syntax-line (cdr p))
                                                        (max 0 (- (syntax-column key) cap))
                                                        (max 1 (- (syntax-position key) cap))
                                                        (+ (syntax-span (cdr p)) (syntax-span key) sep cap cap))))
                                             (values (cons elem l2)
                                                     (+ (syntax-column elem) (syntax-span elem) 2)
                                                     (syntax-line elem))))])
              (cond
                ;; constructed:
                [(and expr? (zero? quote-depth))
                 (define l (apply append
                                  (map (lambda (p)
                                         (let ([p (syntax-e p)])
                                           (list (forced-pair-car p)
                                                 (forced-pair-cdr p))))
                                       (reverse l2))))
                 (datum->syntax
                  #f
                  (cons (datum->syntax #f
                                       hash-type
                                       (vector (syntax-source c)
                                               (syntax-line c)
                                               (+ (syntax-column c) 1)
                                               (+ (syntax-position c) 1)
                                               (string-length (symbol->string hash-type))))
                        l)
                  c)]
                ;; quoted:
                [else
                 (datum->syntax #f (reverse l2) (vector (syntax-source c)
                                                        (syntax-line c)
                                                        (+ (syntax-column c) delta)
                                                        (+ (syntax-position c) delta)
                                                        (max 1 (- (syntax-span c) delta))))]))
            #f)
           (set! src-col (+ orig-col (syntax-span c))))]
        [(graph-reference? (syntax-e c))
         (advance c init-line! srcless-step)
         (out (iformat "#~a#" (unbox (graph-reference-bx (syntax-e c))))
              (if (positive? quote-depth)
                  value-color
                  paren-color))
         (set! src-col (+ src-col (syntax-span c)))]
        [(graph-defn? (syntax-e c))
         (advance c init-line! srcless-step)
         (define bx (graph-defn-bx (syntax-e c)))
         (out (iformat "#~a=" (unbox bx))
              (if (positive? quote-depth)
                  value-color
                  paren-color))
         (set! src-col (+ src-col 3))
         ((loop init-line! quote-depth expr? #f) (graph-defn-r (syntax-e c)) #f)]
        [(and (keyword? (syntax-e c)) expr?)
         (advance c init-line! srcless-step)
         (let ([quote-depth (to-quoted c expr? quote-depth out color? inc-src-col)])
           (typeset-atom c out color? quote-depth expr? escapes? defn?)
           (set! src-col (+ src-col (or (syntax-span c) 1))))]
        [else
         (advance c init-line! srcless-step)
         (typeset-atom c out color? quote-depth expr? escapes? defn?)
         (set! src-col (+ src-col (or (syntax-span c) 1)))
         #;
         (hash-set! next-col-map src-col dest-col)]))
    (out prefix1 #f)
    (set! dest-col 0)
    (hash-set! next-col-map init-col dest-col)
    ((loop (lambda () (set! src-col init-col) (set! dest-col 0)) 0 expr? #f) c #f)
    (if (list? suffix)
        (map (lambda (sfx)
               (finish-line!)
               (out sfx #f))
             suffix)
        (out suffix #f))
    (unless (null? content)
      (finish-line!))
    (if multi-line?
        (if (= 1 (length docs))
            (car docs)
            (make-table block-color (map list (reverse docs))))
        (make-sized-element #f (reverse content) dest-col))))

(define (typeset c multi-line? prefix1 prefix suffix color? expr? escapes? defn? elem-wrap)
  (let* ([c (syntax-ize c 0 #:expr? expr?)]
         [s (syntax-e c)])
    (if (or multi-line?
            (and escapes? (eq? 'code:blank s))
            (pair? s)
            (mpair? s)
            (vector? s)
            (struct? s)
            (box? s)
            (null? s)
            (hash? s)
            (graph-defn? s)
            (graph-reference? s)
            (struct-proxy? s)
            (and expr? (or (identifier? c)
                           (keyword? (syntax-e c)))))
        (gen-typeset c multi-line? prefix1 prefix suffix color? expr? escapes? defn? elem-wrap)
        (typeset-atom c 
                      (letrec ([mk
                                (case-lambda 
                                  [(elem color)
                                   (mk elem color (or (syntax-span c) 1))]
                                  [(elem color len)
                                   (elem-wrap
                                    (if (and (string? elem)
                                             (= len (string-length elem)))
                                        (make-element/cache (and color? color) elem)
                                        (make-sized-element (and color? color) elem len)))])])
                        mk)
                      color? 0 expr? escapes? defn?))))
  
(define (to-element c
                    #:expr? [expr? #f]
                    #:escapes? [escapes? #t]
                    #:defn? [defn? #f])
  (typeset c #f "" "" "" #t expr? escapes? defn? values))

(define (to-element/no-color c
                             #:expr? [expr? #f]
                             #:escapes? [escapes? #t])
  (typeset c #f "" "" "" #f expr? escapes? #f values))

(define (to-paragraph c 
                      #:expr? [expr? #f] 
                      #:escapes? [escapes? #t] 
                      #:color? [color? #t]
                      #:wrap-elem [elem-wrap (lambda (e) e)])
  (typeset c #t "" "" "" color? expr? escapes? #f elem-wrap))

(define ((to-paragraph/prefix pfx1 pfx sfx) c 
                                            #:expr? [expr? #f] 
                                            #:escapes? [escapes? #t] 
                                            #:color? [color? #t]
                                            #:wrap-elem [elem-wrap (lambda (e) e)])
  (typeset c #t pfx1 pfx sfx color? expr? escapes? #f elem-wrap))

(begin-for-syntax 
  (define-struct variable-id (sym) 
    #:omit-define-syntaxes
    #:property prop:procedure (lambda (self stx)
                                (raise-syntax-error
                                 #f
                                 (string-append
                                  "misuse of an identifier (not in `racket', etc.) that is"
                                  " bound as a code-typesetting variable")
                                 stx)))
  (define-struct element-id-transformer (proc) 
    #:omit-define-syntaxes
    #:property prop:procedure (lambda (self stx)
                                (raise-syntax-error
                                 #f
                                 (string-append
                                  "misuse of an identifier (not in `racket', etc.) that is"
                                  " bound as an code-typesetting element transformer")
                                 stx))))

(define-syntax (define-code stx)
  (syntax-case stx ()
    [(_ code typeset-code uncode d->s stx-prop)
     (syntax/loc stx
       (define-syntax (code stx)
         (define (wrap-loc v ctx e)
           `(,#'d->s ,ctx
                     ,e
                     #(code
                       ,(syntax-line v)
                       ,(syntax-column v)
                       ,(syntax-position v)
                       ,(syntax-span v))))
         (define (stx->loc-s-expr/esc v uncode-id)
           (define (stx->loc-s-expr v)
             (let ([slv (and (identifier? v)
                             (syntax-local-value v (lambda () #f)))])
               (cond
                 [(variable-id? slv)
                  (wrap-loc v #f `(,#'make-var-id ',(variable-id-sym slv)))]
                 [(element-id-transformer? slv)
                  (wrap-loc v #f ((element-id-transformer-proc slv) v))]
                 [(syntax? v)
                  (let ([mk (wrap-loc
                             v
                             `(quote-syntax ,(datum->syntax v 'defcode))
                             (syntax-case v ()
                               [(esc e) 
                                (and (identifier? #'esc)
                                     (free-identifier=? #'esc uncode-id))
                                #'e]
                               [_ (stx->loc-s-expr (syntax-e v))]))])
                    (let ([prop (syntax-property v 'paren-shape)])
                      (if prop
                          `(,#'stx-prop ,mk 'paren-shape ,prop)
                          mk)))]
                 [(null? v) 'null]
                 [(list? v) `(list . ,(map stx->loc-s-expr v))]
                 [(pair? v) `(cons ,(stx->loc-s-expr (car v))
                                   ,(stx->loc-s-expr (cdr v)))]
                 [(vector? v) `(vector ,@(map
                                          stx->loc-s-expr
                                          (vector->list v)))]
                 [(and (struct? v) (prefab-struct-key v))
                  `(make-prefab-struct (quote ,(prefab-struct-key v))
                                       ,@(map
                                          stx->loc-s-expr
                                          (cdr (vector->list (struct->vector v)))))]
                 [(box? v) `(box ,(stx->loc-s-expr (unbox v)))]
                 [(hash? v) `(,(cond
                                 [(hash-eq? v) 'make-immutable-hasheq]
                                 [(hash-eqv? v) 'make-immutable-hasheqv]
                                 [(hash-equal? v) 'make-immutable-hash]
                                 [(hash-equal-always? v) 'make-immutable-hashalw]
                                 [else (error 'define-code "unexpected hash table type: ~s" v)])
                              (list
                               ,@(hash-map
                                  v
                                  (lambda (k v)
                                    `(cons (quote ,k)
                                           ,(stx->loc-s-expr v))))))]
                 [else `(quote ,v)])))
           (stx->loc-s-expr v))
         (define (cvt s uncode-id)
           (datum->syntax #'here (stx->loc-s-expr/esc s uncode-id) #f))
         (if (eq? (syntax-local-context) 'expression)
             (syntax-case stx ()
               [(_ #:escape uncode-id expr) #`(typeset-code #,(cvt #'expr #'uncode-id))]
               [(_ expr) #`(typeset-code #,(cvt #'expr #'uncode))]
               [(_ #:escape uncode-id expr (... ...))
                #`(typeset-code #,(cvt #'(code:line expr (... ...)) #'uncode-id))]
               [(_ expr (... ...))
                #`(typeset-code #,(cvt #'(code:line expr (... ...)) #'uncode))])
             (quasisyntax/loc stx
               (#%expression #,stx)))))]
    [(_ code typeset-code uncode d->s)
     #'(define-code code typeset-code uncode d->s syntax-property)]
    [(_ code typeset-code uncode)
     #'(define-code code typeset-code uncode datum->syntax syntax-property)]
    [(_ code typeset-code) #'(define-code code typeset-code unsyntax)]))

  
(define syntax-ize-hook (make-parameter (lambda (v col) #f)))

(define (vector->short-list v extract)
  (vector->list v)
  #;
  (let ([l (vector->list v)])
    (reverse (list-tail
              (reverse l)
              (- (vector-length v)
                 (let loop ([i (sub1 (vector-length v))])
                   (cond
                     [(zero? i) 1]
                     [(eq? (extract (vector-ref v i))
                           (extract (vector-ref v (sub1 i))))
                      (loop (sub1 i))]
                     [else (add1 i)])))))))

(define (short-list->vector v l)
  (list->vector
   (let ([n (length l)])
     (if (n . < . (vector-length v))
         (reverse (let loop ([r (reverse l)][i (- (vector-length v) n)])
                    (if (zero? i)
                        r
                        (loop (cons (car r) r) (sub1 i)))))
         l))))

(define-struct var-id (sym))
(define-struct shaped-parens (val shape))
(define-struct long-boolean (val))
(define-struct just-context (val ctx))
(define-struct alternate-display (id string))
(define-struct literal-syntax (stx))
(define-struct struct-proxy (name content))

(define-struct graph-reference (bx))
(define-struct graph-defn (r bx))

(define (syntax-ize v col [line 1] #:expr? [expr? #f])
  (do-syntax-ize v col line (box #hasheq()) #f (and expr? 0) #f))

(define (graph-count ht graph?)
  (and graph?
       (let ([n (hash-ref (unbox ht) '#%graph-count 0)])
         (set-box! ht (hash-set (unbox ht) '#%graph-count (add1 n)))
         n)))

(define-struct forced-pair (car cdr))

(define (quotable? v)
  (define graph (make-hasheq))
  (let quotable? ([v v])
    (cond
      [(hash-ref graph v #f) #t]
      [else
       (hash-set! graph v #t)
       (cond
         [(syntax? v) (quotable? (syntax-e v))]
         [(pair? v) (and (quotable? (car v))
                         (quotable? (cdr v)))]
         [(vector? v) (andmap quotable? (vector->list v))]
         [(hash? v) (for/and ([(k v) (in-hash v)])
                      (and (quotable? k)
                           (quotable? v)))]
         [(box? v) (quotable? (unbox v))]
         [(and (struct? v)
               (prefab-struct-key v))
          (andmap quotable? (vector->list (struct->vector v)))]
         [(struct? v) (if (custom-write? v)
                          (case (or (and (custom-print-quotable? v)
                                         (custom-print-quotable-accessor v))
                                    'self)
                            [(self always) #t]
                            [(never) #f]
                            [(maybe)
                             (andmap quotable? (vector->list (struct->vector v)))])
                          #f)]
         [(struct-proxy? v) #f]
         [(mpair? v) #f]
         [else #t])])))

(define (do-syntax-ize v col line ht graph? qq no-cons?)
  (cond
    [((syntax-ize-hook) v col)
     => (lambda (r) r)]
    [(shaped-parens? v)
     (syntax-property (do-syntax-ize (shaped-parens-val v) col line ht #f qq #f)
                      'paren-shape
                      (shaped-parens-shape v))]
    [(long-boolean? v)
     (datum->syntax #f
                    (and (long-boolean-val v) #t) 
                    (vector #f line col (+ 1 col) (if (long-boolean-val v) 5 6)))]
    [(just-context? v)
     (define s (do-syntax-ize (just-context-val v) col line ht #f qq #f))
     (datum->syntax (just-context-ctx v)
                    (syntax-e s)
                    s
                    s)]
    [(alternate-display? v)
     (define s (do-syntax-ize (alternate-display-id v) col line ht #f qq #f))
     (syntax-property s
                      'display-string
                      (alternate-display-string v))]
    [(hash-ref (unbox ht) v #f)
     => (lambda (m)
          (unless (unbox m)
            (set-box! m #t))
          (datum->syntax #f
                         (make-graph-reference m)
                         (vector #f line col (+ 1 col) 1)))]
    [(and qq 
          (zero? qq)
          (or (pair? v)
              (forced-pair? v)
              (vector? v)
              (hash? v)
              (box? v)
              (and (struct? v)
                   (prefab-struct-key v)))
          (quotable? v)
          (not no-cons?))
     ;; Add a quote:
     (define l (do-syntax-ize v (add1 col) line ht #f 1 #f))
     (datum->syntax #f
                    (syntax-e l)
                    (vector (syntax-source l)
                            (syntax-line l)
                            (sub1 (syntax-column l))
                            (max 0 (sub1 (syntax-position l)))
                            (add1 (syntax-span l))))]
    [(and (list? v)
          (pair? v)
          (or (not qq)
              (positive? qq)
              (quotable? v))
          (let ([s (let ([s (car v)])
                     (if (just-context? s)
                         (just-context-val s)
                         s))])
            (memq s '(quote unquote unquote-splicing)))
          (not no-cons?))
     => (lambda (s)
          (define delta (if (and qq (zero? qq))
                            1
                            0))
          (define c (do-syntax-ize (cadr v) (+ col delta) line ht #f qq #f))
          (datum->syntax #f
                         (list (do-syntax-ize (car v) col line ht #f qq #f)
                               c)
                         (vector #f line col (+ 1 col)
                                 (+ delta
                                    (syntax-span c)))))]
    [(or (list? v)
         (vector? v)
         (and (struct? v)
              (or (and qq 
                       ;; Watch out for partially transparent subtypes of `element'
                       ;;  or convertible values:
                       (not (convertible? v))
                       (not (element? v)))
                  (prefab-struct-key v))))
     (define orig-ht (unbox ht))
     (define graph-box (box (graph-count ht graph?)))
     (set-box! ht (hash-set (unbox ht) v graph-box))
     (define graph-sz (if graph?
                          (+ 2 (string-length (format "~a" (unbox graph-box))))
                          0))
     (define vec-sz (cond
                      [(vector? v)
                       (if (and qq (zero? qq)) 0 1)]
                      [(struct? v)
                       (if (and (prefab-struct-key v)
                                (or (not qq) (positive? qq)))
                           2
                           0)]
                      [else 0]))
     (define delta (if (and qq (zero? qq))
                       (cond
                         [(vector? v) 8] ; `(vector '
                         [(struct? v) 1] ; '('
                         [no-cons? 1]    ; '('
                         [else 6])       ; `(list '
                       1))
     (define r (let ([l (let loop ([col (+ col delta vec-sz graph-sz)]
                                   [v (cond
                                        [(vector? v)
                                         (vector->short-list v values)]
                                        [(struct? v)
                                         (cons (let ([pf (prefab-struct-key v)])
                                                 (if pf
                                                     (prefab-struct-key v)
                                                     (object-name v)))
                                               (cdr (vector->list (struct->vector v qq-ellipses))))]
                                        [else v])])
                          (cond
                            [(null? v) null]
                            [else
                             (define i (do-syntax-ize (car v) col line ht #f qq #f))
                             (cons i (loop (+ col 1 (syntax-span i)) (cdr v)))]))])
                 (datum->syntax #f
                                (cond
                                  [(vector? v) (short-list->vector v l)]
                                  [(struct? v)
                                   (define pf (prefab-struct-key v))
                                   (if pf
                                       (apply make-prefab-struct (prefab-struct-key v) (cdr l))
                                       (make-struct-proxy (car l) (cdr l)))]
                                  [else l])
                                (vector #f line
                                        (+ graph-sz col)
                                        (+ 1 graph-sz col)
                                        (+ 1
                                           vec-sz
                                           delta
                                           (if (zero? (length l))
                                               0
                                               (sub1 (length l)))
                                           (apply + (map syntax-span l)))))))
     (unless graph?
       (set-box! ht (hash-set (unbox ht) v #f)))
     (cond
       [graph? (datum->syntax #f
                              (make-graph-defn r graph-box)
                              (vector #f (syntax-line r)
                                      (- (syntax-column r) graph-sz)
                                      (- (syntax-position r) graph-sz)
                                      (+ (syntax-span r) graph-sz)))]
       [(unbox graph-box)
        ;; Go again, this time knowing that there will be a graph:
        (set-box! ht orig-ht)
        (do-syntax-ize v col line ht #t qq #f)]
       [else r])]
    [(or (pair? v)
         (mpair? v)
         (forced-pair? v))
     (define carv
       (cond
         [(pair? v) (car v)]
         [(mpair? v) (mcar v)]
         [else (forced-pair-car v)]))
     (define cdrv
       (cond
         [(pair? v) (cdr v)]
         [(mpair? v) (mcdr v)]
         [else (forced-pair-cdr v)]))
     (define orig-ht (unbox ht))
     (define graph-box (box (graph-count ht graph?)))
     (set-box! ht (hash-set (unbox ht) v graph-box))
     (define delta
       (if (and qq (zero? qq) (not no-cons?))
           (cond
             [(mpair? v) 7] ; "(mcons "
             [(or (list? cdrv) (not (pair? cdrv))) 6] ; "(cons "
             [else 7]) ; "(list* "
           1))
     (define inc (if graph?
                     (+ 2 (string-length (format "~a" (unbox graph-box))))
                     0))
     (define a (do-syntax-ize carv (+ col delta inc) line ht #f qq #f))
     (define sep
       (cond
         [(and (pair? v)
               (pair? cdrv)
               ;; FIXME: what if it turns out to be a graph reference?
               (not (hash-ref (unbox ht) cdrv #f)))
          0]
         [(and qq (zero? qq)) 1]
         [else 3]))
     (define b (do-syntax-ize cdrv (+ col delta inc (syntax-span a) sep) line ht #f qq #t))
     (define r (datum->syntax #f
                              (if (mpair? v)
                                  (mcons a b)
                                  (cons a b))
                              (vector #f line (+ col inc) (+ delta col inc)
                                      (+ 1 delta
                                         (if (and qq (zero? qq)) 1 0)
                                         sep (syntax-span a) (syntax-span b)))))
     (unless graph?
       (set-box! ht (hash-set (unbox ht) v #f)))
     (cond
       [graph? (datum->syntax #f
                              (make-graph-defn r graph-box)
                              (vector #f line col (+ delta col)
                                      (+ inc (syntax-span r))))]
       [(unbox graph-box)
        ;; Go again...
        (set-box! ht orig-ht)
        (do-syntax-ize v col line ht #t qq #f)]
       [else r])]
    [(box? v)
     (define delta (if (and qq (zero? qq))
                       5 ; "(box "
                       2)) ; "#&"
     (define a (do-syntax-ize (unbox v) (+ col delta) line ht #f qq #f))
     (datum->syntax #f
                    (box a)
                    (vector #f line col (+ 1 col)
                            (+ delta (if (and qq (zero? qq)) 1 0) (syntax-span a))))]
    [(hash? v)
     (define delta
       (cond
         [(hash-eq? v) 7]
         [(hash-eqv? v) 8]
         [else 6]))
     (define undelta
       (if (and qq (zero? qq))
           (- delta 1)
           0))
     (define pairs
       (cond
         [(and qq (zero? qq))
          (define ls (do-syntax-ize (apply append (hash-map v (lambda (k v) (list k v))))
                                    (+ col delta -1) line ht #f qq #t))
          (datum->syntax
           #f
           (let loop ([l (syntax->list ls)])
             (if (null? l)
                 null
                 (cons (cons (car l) (cadr l)) (loop (cddr l)))))
           ls)]
         [else (do-syntax-ize (hash-map v make-forced-pair) (+ col delta) line ht #f qq #f)]))
     (datum->syntax #f
                    ((cond
                       [(hash-eq? v) make-immutable-hasheq]
                       [(hash-eqv? v) make-immutable-hasheqv]
                       [else make-immutable-hash])
                     (map (lambda (p)
                            (let ([p (syntax-e p)])
                              (cons (syntax->datum (car p))
                                    (cdr p))))
                          (syntax->list pairs)))
                    (vector (syntax-source pairs)
                            (syntax-line pairs)
                            (max 0 (- (syntax-column pairs) undelta))
                            (max 1 (- (syntax-position pairs) undelta))
                            (+ (syntax-span pairs) undelta)))]
    [else
     (datum->syntax #f v (vector #f line col (+ 1 col) 1))]))

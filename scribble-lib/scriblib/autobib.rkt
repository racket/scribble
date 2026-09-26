#lang at-exp racket/base
(require scribble/manual
         racket/list
         racket/date
         racket/class
         racket/match
         scribble/core
         scribble/decode
         scribble/html-properties
         scribble/latex-properties
         scribble/typst-properties
         (for-syntax syntax/parse
                     racket/base)
         racket/string
         setup/main-collects
         racket/contract)

(provide define-cite
         author+date-style author+date-square-bracket-style number-style
         make-bib in-bib (rename-out [auto-bib? bib?])
         author-name org-author-name
         (contract-out
          [authors (->* (content?) #:rest (listof content?) element?)]
          [proceedings-location
           (->* [any/c] [#:pages (or/c (list/c any/c any/c) #f)
                         #:series any/c #:volume any/c #:number any/c
                         #:editor any/c #:address any/c #:publisher any/c #:organization any/c]
                (or/c content? #f))]
          [journal-location
           (->* [any/c] [#:pages (or/c (list/c any/c any/c) #f) #:volume any/c #:number any/c]
                (or/c content? #f))]
          [book-location
           (->* []
                [#:edition any/c #:chapter any/c #:editor any/c
                 #:series any/c #:volume any/c #:number any/c #:pages (or/c (list/c any/c any/c) #f)
                 #:publisher any/c #:address any/c] (or/c content? #f))]
          [booklet-location
           (->* [] [#:howpublished any/c #:address any/c] (or/c content? #f))]
          [misc-location
           (->* [] [#:howpublished any/c] (or/c content? #f))]
          [techrpt-location
           (->* [#:institution any/c] [#:number any/c #:type any/c #:address any/c]
                (or/c content? #f))]
          [dissertation-location
           (->* [#:institution any/c] [#:degree any/c #:type any/c #:address any/c]
                content?)]
          [book-chapter-location
           (->* [any/c]
                [#:edition any/c #:editor any/c #:chapter any/c
                 #:series any/c #:volume any/c #:number any/c #:pages (or/c (list/c any/c any/c) #f)
                 #:publisher any/c #:address any/c]
                (or/c content? #f))]
          [webpage-location
           (->* [] [string? #:accessed any/c] (or/c content? #f))]
          [manual-location
           (->* [] [#:organization any/c #:edition any/c] (or/c content? #f))])
         other-authors
         editor
         abbreviate-given-names

         #; (String[url] -> Element)
         url-rendering

         doi-rendering)

(define abbreviate-given-names (make-parameter #f))
(define url-rendering (make-parameter (λ (url) (link url (make-element 'url (list url))))))
(define doi-rendering
  (make-parameter (λ (doi) (make-element "pseudodoi"
                                         (list "doi:"
                                               (link (string-append
                                                      "https://doi.org/"
                                                      doi) doi))))))

(define autobib-style-extras
  (let ([abs (lambda (s)
               (path->main-collects-relative
                (collection-file-path s "scriblib")))])
    (list
     (make-css-addition (abs "autobib.css"))
     (make-tex-addition (abs "autobib.tex"))
     (make-typ-addition (abs "autobib.typ")))))

(define bib-single-style (make-style "AutoBibliography" autobib-style-extras))

(define bibentry-style
  (make-style "Autobibentry"
              (cons (alt-tag "div") autobib-style-extras)))
(define colbibentry-style
  (make-style "Autocolbibentry"
              (cons (alt-tag "div") autobib-style-extras)))
(define bibentrytarget-style (make-style "Autobibtarget" autobib-style-extras))
(define colbibnumber-style (make-style "Autocolbibnumber" autobib-style-extras))

(define-struct auto-bib (author date title location url note is-book? doi key specific))
(define-struct bib-group (ht))

(define-struct (author-element element) (names cite)) ;; NB: names should always be a string
(define-struct (other-author-element author-element) ())

(define (author-element-names* x)
  (and x (author-element-names x)))

;; render the use of a citation.
(define (add-cite group bib-entry which with-specific? disambiguation style)
  (let ([key (auto-bib-key bib-entry)])
    (when disambiguation
      (for ([bib disambiguation])
        (hash-set! (bib-group-ht group) (auto-bib-key bib) bib)))
    (hash-set! (bib-group-ht group) key bib-entry)
    (make-delayed-element
     (lambda (renderer part ri)
       ;; (list which key) should be mapped to the bibliography element.
       (define s (resolve-get part ri `(,which ,key)))
       (define content
         (list (or s "???")
               (cond [(not (send style disambiguate-date?)) '()]
                     [disambiguation ;; should be a list of bib-entries with same author/date
                      (define disambiguation*
                        (add-between (for/list ([bib (in-list disambiguation)])
                                       (define key (auto-bib-key bib))
                                       (define maybe-disambiguation
                                         (resolve-get part ri `(autobib-disambiguation ,key)))
                                       (case maybe-disambiguation
                                         [(#f) #f]
                                         [(unambiguous) #f]
                                         [else (make-link-element "AutobibLink" maybe-disambiguation `(autobib ,key))]))
                                     ","))
                      (cond [(not (car disambiguation*)) '()] ;; the bib was unambiguous
                            [else disambiguation*])]
                     [else '()])
               (if with-specific?
                   (auto-bib-specific bib-entry)
                   "")))
       (make-link-element "AutobibLink"
                          content
                          `(autobib ,(auto-bib-key bib-entry))))
     (lambda () "(???)")
     (lambda () "(???)"))))

(define (add-date-cites group bib-entries delimiter style sort? maybe-date<? maybe-date=?)
  (define date<? (or maybe-date<? default-date<?))
  (define date=? (or maybe-date=? default-date=?))
  (define sorted-by-date (if sort?
                             (sort bib-entries date<?)
                             bib-entries))
  (define partitioned-by-ambiguity
    (let-values ([(last last-ambiguous-list partition)
                  (for/fold ([last #f]
                             [currently-ambiguous '()]
                             [partition '()])
                      ([bib (reverse sorted-by-date)])
                    (cond [(and (send style collapse-for-date?)
                                last (date=? last bib)
                                (equal? (auto-bib-specific bib) "")
                                (equal? (auto-bib-specific last) ""))
                           ;; can group
                           (values bib (cons bib currently-ambiguous) partition)]
                          ;; first element.
                          [(not last) (values bib (list bib) partition)]
                          ;; not ambiguous. Start next group.
                          [else (values bib (list bib) (cons currently-ambiguous partition))]))])
      (cons last-ambiguous-list partition)))
  (cond [(null? bib-entries) '()]
        [else
         (add-between
          (for/list ([part (in-list partitioned-by-ambiguity)])
            (add-cite group (car part) 'autobib-date #t part style))
          delimiter)]))

(define all-equal?
  (case-lambda
   [(a) #t]
   [(a b) (equal? a b)]
   [(a . bs) (andmap (lambda (v) (equal? a v)) bs)]))

(define (add-inline-cite group bib-entries style bib-date<? bib-date=?)
  (for ([i bib-entries])
    (hash-set! (bib-group-ht group) (auto-bib-key i) i))
  (when (and (pair? (cdr bib-entries))
             (not (apply all-equal? (map (compose author-element-names* auto-bib-author) bib-entries))))
    (error 'citet "citet must be used with identical authors, given ~a"
           (map (compose author-element-names* auto-bib-author) bib-entries)))
  (make-element
   (make-style "Autobibref" '())
   (list (add-cite group (car bib-entries) 'autobib-author #f #f style)
         'nbsp
         (send style get-cite-open)
         (add-date-cites group bib-entries
                         (send style get-group-sep)
                         style #t bib-date<? bib-date=?)
         (send style get-cite-close))))

;; This allows citing multiple sources in one @cite. Groups of citations are separated by semicolons.
(define (add-cites group bib-entries sort? style bib-date<? bib-date=?)
  (define-values (groups keys)
    (for/fold ([h (hash)] [ks null]) ([b (reverse bib-entries)])
      (let ([k (author-element-names* (auto-bib-author b))])
        (values (hash-update h k (lambda (cur) (cons b cur)) null)
                (cons k (remove k ks))))))
  (make-element
   (make-style "Autobibref" autobib-style-extras)
   (append
    (list 'nbsp (send style get-cite-open))
    (add-between
     (for/list ([k (if sort? (sort keys (lambda (x y) (if (not (and x y)) x (string-ci<? x y)))) keys)])
       (let ([v (hash-ref groups k)])
         (make-element
          #f
          (send style
                render-author+dates
                (add-cite group (car v) 'autobib-author #f #f style)
                (add-date-cites group v (send style get-item-sep) style sort? bib-date<? bib-date=?)))))
     (send style get-group-sep))
    (list (send style get-cite-close)))))

(define (extract-bib-author b)
  (or (auto-bib-author b)
      (org-author-name (auto-bib-title b))))

(define (extract-bib-key b)
  (author-element-names (extract-bib-author b)))

;; Defaults only care about the year.
(define (default-render-date-bib date)
  (make-element #f (list (number->string (date-year date)))))
(define (default-render-date-cite date)
  (make-element #f (list (number->string (date-year date)))))
(define (default-date<? b0 b1)
  (and (auto-bib-date b0) (auto-bib-date b1)
       (< (date-year (auto-bib-date b0)) (date-year (auto-bib-date b1)))))
(define (default-date=? b0 b1)
  (and (auto-bib-date b0) (auto-bib-date b1)
       (= (date-year (auto-bib-date b0)) (date-year (auto-bib-date b1)))))

;; 0 -> a, 1 -> b, etc.
(define (default-disambiguation n)
  (when (>= n 26)
    (error 'default-disambiguation "Citations too ambiguous for default disambiguation scheme."))
  (make-element #f (list (format "~a" (integer->char (+ 97 n))))))

(define author+date-style%
  (class object%
    (define/public (bibliography-table-style) bib-single-style)
    (define/public (entry-style) bibentry-style)
    (define/public (disambiguate-date?) #t)
    (define/public (collapse-for-date?) #t)
    (define/public (get-cite-open) "(")
    (define/public (get-cite-close) ")")
    (define/public (get-group-sep) "; ")
    (define/public (get-item-sep) ", ")
    (define/public (render-citation date-cite i) date-cite)
    (define/public (render-author+dates author dates) (list* author " " dates))
    (define/public (bibliography-line i e) (list e))
    (define/public (bibliography-prefix i) null)
    (super-new)))

(define author+date-style (new author+date-style%))

(define author+date-square-bracket-style
  (new
   (class author+date-style%
     (define/override (get-cite-open) "[")
     (define/override (get-cite-close) "]")
     (super-new))))

(define number-style
  (new
   (class object%
     (define/public (bibliography-table-style) bib-single-style)
     (define/public (entry-style) colbibentry-style)
     (define/public (disambiguate-date?) #f)
     (define/public (collapse-for-date?) #f)
     (define/public (get-cite-open) "[")
     (define/public (get-cite-close) "]")
     (define/public (get-group-sep) ", ")
     (define/public (get-item-sep) ", ")
     (define/public (render-citation date-cite i) (number->string i))
     (define/public (render-author+dates author dates) dates)
     (define/public (bibliography-prefix i)
       (make-element
        colbibnumber-style
        (list "[" (number->string i) "]" 'nbsp)))
     (define/public (bibliography-line i e)
       (list e))
     (super-new))))

(define (gen-bib tag group sec-title
                 style maybe-disambiguator
                 maybe-render-date-bib maybe-render-date-cite
                 maybe-date<? maybe-date=?
                 spaces)
  (define disambiguator (or maybe-disambiguator default-disambiguation))
  (define date<? (or maybe-date<? default-date<?))
  (define date=? (or maybe-date=? default-date=?))
  (define render-date-bib (or maybe-render-date-bib default-render-date-bib))
  (define render-date-cite (or maybe-render-date-cite default-render-date-cite))
  (define (author/date<? a b)
    ;; Compare author names, then date, then full key
    (or (string-ci<? (extract-bib-key a) (extract-bib-key b))
        (and (string-ci=? (extract-bib-key a) (extract-bib-key b))
             (cond
              [(not (auto-bib-date a))
               (if (auto-bib-date b)
                   #f
                   (string-ci<? (auto-bib-key a) (auto-bib-key b)))]
              [(not (auto-bib-date b)) #t]
              [(date<? a b) #t]
              [(date<? b a) #f]
              [else (string-ci<? (auto-bib-key a) (auto-bib-key b))]))))
  (define (ambiguous? a b)
    (and (string-ci=? (content->string (author-element-cite (extract-bib-author a)))
                      (content->string (author-element-cite (extract-bib-author b))))
         (auto-bib-date a)
         (auto-bib-date b)
         (date=? a b)))
  (define bibs (sort (hash-values (bib-group-ht group))
                     author/date<?))
  (define disambiguated
    (let ()
      (define (bib->para bib disambiguation i)
        ;; Communicate to scribble's resolve step.
        (define (collect ci)
          ;; store the author
          (collect-put! ci
                        `(autobib-author ,(auto-bib-key bib)) ;; (list which key)
                        (make-element
                         #f
                         (list (author-element-cite (extract-bib-author bib)))))
          ;; store the date
          (when (auto-bib-date bib)
            (collect-put! ci
                          `(autobib-date ,(auto-bib-key bib)) ;; (list which key)
                          (make-element #f (list
                                            (send style
                                                  render-citation
                                                  (render-date-cite (auto-bib-date bib))
                                                  i)))))
          ;; store how to disambiguate it from other like citations.
          (collect-put! ci
                        `(autobib-disambiguation ,(auto-bib-key bib))
                        (or disambiguation 'unambiguous)))
        (define entry
          (bib->entry bib style disambiguation render-date-bib i))
        (define blocks (compound-paragraph-blocks entry))
        (define first-block (car blocks))
        (define marked-first
          (make-paragraph
           (paragraph-style first-block)
           (list
            (send style bibliography-prefix i)
            (make-collect-element
             bibentrytarget-style
             (list
              (make-target-element
               #f
               (paragraph-content first-block)
               `(autobib ,(auto-bib-key bib))))
             collect))))
        (send style
              bibliography-line
              i
              (make-compound-paragraph
               (compound-paragraph-style entry)
               (cons marked-first (cdr blocks)))))
      ;; create the bibliography with disambiguations added.
      (define-values (last num-ambiguous rev-disambiguated*)
        (for/fold ([last #f] [num-ambiguous 0] [rev-disambiguated '()]) ([bib (in-list bibs)]
                                                                         [i (in-naturals 1)])
          (define ambiguous?? (and (send style disambiguate-date?)
                                   last
                                   (ambiguous? last bib)))
          (define num-ambiguous*
            (cond [ambiguous?? (add1 num-ambiguous)]
                  [else 0]))
          ;; the current entry is ambiguous with the last. Modify the last
          ;; to have the first disambiguation.
          (define rev-disambiguated*
            (cond [(and ambiguous?? (= 0 num-ambiguous))
                   (cons (bib->para last (disambiguator num-ambiguous) i)
                         (cdr rev-disambiguated))]
                  [else rev-disambiguated]))
          (define para*
            (bib->para bib (and ambiguous?? (disambiguator num-ambiguous*)) i))
          (values bib num-ambiguous* (cons para* rev-disambiguated*))))
      (reverse rev-disambiguated*)))

  (define (make-space)
    (list
      (make-paragraph (make-style #f '()) '(""))
      (make-paragraph (make-style #f '()) '(""))))

  (define table
    (make-table (send style bibliography-table-style)
                               (add-between #:splice? #t
                                            disambiguated
                                            (for/list ([i (in-range 1 spaces)])
                                              (make-space)))))

  (if sec-title
    (make-part #f
               `((part ,tag))
               (list sec-title)
               (make-style #f '(unnumbered))
               null
               (list table)
               null)
    table))

;; Build a potentially multi-paragraph entry base on the note field.
(define (bib->entry bib style disambiguation render-date-bib i)
  (define-values (author date title location url note is-book? doi)
    (values (auto-bib-author bib)
            (auto-bib-date bib)
            (auto-bib-title bib)
            (auto-bib-location bib)
            (auto-bib-url bib)
            (auto-bib-note bib)
            (auto-bib-is-book? bib)
            (auto-bib-doi bib)))
  (define note-blocks
    (if (and note
             (not (string=? "" (string-trim (content->string note)))))
      (note->flow note)
      null))
  (define header
    (append
     (if author
         `(,author
           ,@(if (ends-in-punc? author)
                 '(" ")
                 '(". ")))
         null)
     ;; (if is-book? null '(ldquo))
     (if is-book?
         (list (italic title))
         (decode-content (list title)))
     (if (ends-in-punc? title)
         null
         '("."))
     ;; (if is-book? null '(rdquo))
     (if location
         `(" " ,@(decode-content (list location)) ,(if date "," "."))
         null)
     (if date `(" "
                ,@(if disambiguation
                      `(,@(decode-content (list (render-date-bib date))) ,disambiguation)
                      (decode-content (list (render-date-bib date))))
                ".")
         null)
     (cond
      (doi
       `(" " ,[(doi-rendering) doi]
         ,@(if (pair? note-blocks) '(".") null)))
      (url
       `(" " ,[(url-rendering) url])) ;; do NOT include a . immediately after URL, it's confusing
      (else
       null))))
  (define first-content
    (append header
            (if (pair? note-blocks)
                (cons " " (paragraph-content (car note-blocks)))
                null)))
  (make-compound-paragraph
   (send style entry-style)
   (cons
    (make-paragraph plain first-content)
    (for/list ([p (in-list (if (pair? note-blocks)
                               (cdr note-blocks)
                               null))])
              (make-paragraph
               (make-style #f '(never-indents))
               (paragraph-content p))))))

(define (note->flow note)
  (define (split-lines c)
    (cond
      [(string? c)
       (add-between
        (string-split c "\n" #:trim? #f #:repeat? #f)
        "\n")]
      [(list? c)
       (append-map split-lines c)]
      [else (list c)]))
  (decode-flow (split-lines note)))

(define-syntax (define-cite stx)
  (syntax-parse stx
    [(_ (~var ~cite id) citet:id generate-bibliography:id
        (~or (~optional (~seq #:style style) #:defaults ([style #'author+date-style]))
             (~optional (~seq #:disambiguate fn) #:defaults ([fn #'#f]))
             (~optional (~seq #:render-date-in-bib render-date-bib) #:defaults ([render-date-bib #'#f]))
             (~optional (~seq #:spaces spaces) #:defaults ([spaces #'1]))
             (~optional (~seq #:render-date-in-cite render-date-cite) #:defaults ([render-date-cite #'#f]))
             (~optional (~seq #:date<? date<?) #:defaults ([date<? #'#f]))
             (~optional (~seq #:date=? date=?) #:defaults ([date=? #'#f]))
             (~optional (~seq #:cite-author cite-author:id) #:defaults ([cite-author #'#f]))
             (~optional (~seq #:cite-year cite-year:id) #:defaults ([cite-year #'#f]))) ...)
     (quasisyntax/loc stx
       (begin
         (define group (make-bib-group (make-hasheq)))
         (define the-style style)
         (define (~cite #:sort? [sort? #t] bib-entry . bib-entries)
           (add-cites group (cons bib-entry bib-entries) sort? the-style date<? date=?))
         (define (citet bib-entry . bib-entries)
           (add-inline-cite group (cons bib-entry bib-entries) the-style date<? date=?))
         (define (generate-bibliography #:tag [tag "doc-bibliography"] #:sec-title [sec-title "Bibliography"])
           (gen-bib tag group sec-title the-style fn render-date-bib render-date-cite date<? date=? spaces))
         #,(when (identifier? #'cite-author)
             #'(define (cite-author bib-entry)
                 (add-cite group bib-entry 'autobib-author #f #f the-style)))
         #,(when (identifier? #'cite-year)
             #'(define (cite-year bib-entry . bib-entries)
                 (add-date-cites group (cons bib-entry bib-entries)
                                 (send the-style get-group-sep)
                                 the-style #t date<? date=?)))))]))

(define (ends-in-punc? e)
  (regexp-match? #rx"[.!?,]$" (content->string e)))

(define (understand-date inp)
  ;; Currently there is no string->date function.
  ;; Common usage of autobib has assumed that this should be the year.
  (cond [(or (string? inp) (number? inp))
         (define year
           (cond [(string? inp) (string->number inp)]
                 [else inp]))
         (date 0 0 0 1 1 ;; second/minute/hour/day/month
               year
               ;; week-day/year-day/daylight savings time?/timezone offset
               0 0 #f 0)]
        [(date? inp) inp]
        [(not inp) #f] ;; no date is fine too.
        [else (error 'make-bib "Not given a value that represents a date.")]))

;; We delay making the element for the bib-entry because we may need to add
;; disambiguations during gen-bib.
(define (make-bib #:title title
                  #:author [author #f]
                  #:type [type #f]
                  #:is-book? [is-book? #f]
                  #:location [location #f]
                  #:date [date #f]
                  #:url [url #f]
                  #:doi [doi #f]
                  #:note [note #f])
  ;; TODO what to do with type??
  (define author*
    (cond [(not author) #f]
          [(author-element? author) author]
          [else (parse-author author)]))
  (define parsed-date (understand-date date))
  (make-auto-bib author* parsed-date title location url note is-book? doi
                 (content->string
                  (make-element #f
                                (append
                                 (if author* (list author*) null)
                                 (list title)
                                 (if location (decode-content (list location)) null)
                                 (if date (decode-content (list (default-render-date-bib parsed-date))) null)
                                 (if (and (not doi) url) (list [(url-rendering) url]) null)
                                 (if doi (list [(doi-rendering) doi]) null)
                                 (if note (list note) null))))
                 ""))

(define (in-bib bib where)
  (make-auto-bib
   (auto-bib-author bib)
   (auto-bib-date bib)
   (auto-bib-title bib)
   (auto-bib-location bib)
   (auto-bib-url bib)
   (auto-bib-note bib)
   (auto-bib-is-book? bib)
   (auto-bib-doi bib)
   (auto-bib-key bib)
   ;; "where" is the only specific part of auto-bib elements currently.
   (string-append (auto-bib-specific bib) where)))

(define (parse-author a)
  (cond [(author-element? a) a]
        [else
         (define s (content->string a)) ;; plain text rendering
         (define m (regexp-match #px"^(.*) (([\\-]|\\p{L})+)$" s))
         (define given-names (and m (cadr m)))
         (define family-name (and m (caddr m)))
         (define names
           (cond [m (string-append family-name " " given-names)]
                 [else s]))
         (define cite
           (cond [m (caddr m)]
                 [else s]))
         (define element-content
           (cond
             [(and given-names (abbreviate-given-names))
              (string-append (given-names->initials given-names) family-name)]
             [else a]))
         (make-author-element #f (list element-content) names cite)]))

(define (given-names->initials str)
  (regexp-replace* #rx"(.)[^ ]*( |$)" str "\\1. "))

;; return content for v, preserve false
(define (contentify v)
  (if (or (not v) (content? v))
      v
      (format "~a" v)))
;; return string for v, preserve false
(define (stringify v)
  (and v (content->string (contentify v))))

(module+ test
  (require rackunit)
  (check-equal? (given-names->initials "Matthew") "M. ")
  (check-equal? (given-names->initials "Matthew R.") "M. R. ")
  (check-equal? (given-names->initials "Matthew Raymond") "M. R. ")
  (check-equal? (content->string (journal-location (bold "Journal of Things"))) "Journal of Things")
  (check-equal? (content->string (author-name (italic "Ada") "Lovelace")) "Ada Lovelace")

  (check-false (contentify #f))
  (check-false (stringify #f))
  (check-equal? (contentify 42) "42")
  (check-equal? (stringify 42) "42")
  (define emphasized (italic "foo"))
  (check-eq? (contentify emphasized) emphasized)
  (check-equal? (stringify emphasized) "foo")

  (check-false (flatten-content '(#f "" () #f)))
  (check-equal? (flatten-content '(("a") #f ("b" "c")))
                '("a" "b" "c"))
  (check-equal?
   (concatenate-content #:separator ", "
                        '("foo" "bar") #f "baz")
   '("foo" "bar" ", " "baz"))

  (check-equal? (flatten-content 'foo) "foo")
  (check-equal? (flatten-content 42) "42")
  (check-equal? (content->string (book-location #:edition 'second))
                "Second edition")

  (check-equal? (capitalize-content "second") "Second")
  (check-equal? (capitalize-content '("" "second")) "Second")
  (define emphasized-second (emph "second"))
  (check-eq? (capitalize-content emphasized-second)
             emphasized-second)
  (check-equal?
   (capitalize-content (list emphasized-second " edition"))
   (list emphasized-second " edition"))
  (check-equal?
   (content->string (book-location #:edition "second"))
   "Second edition")

  (define no-note
    (make-bib #:title "Title" #:doi "10.1234/foo"))
  (define with-note
    (make-bib #:title "Title"
              #:doi "10.1234/foo"
              #:note "A note"))
  (define (entry-first-text bib)
    (content->string
     (paragraph-content
      (car (compound-paragraph-blocks
            (bib->entry bib author+date-style #f
                        default-render-date-bib 1))))))
  (check-equal? (entry-first-text no-note)
                "Title. doi:10.1234/foo")
  (check-equal? (entry-first-text with-note)
                "Title. doi:10.1234/foo. A note")
  (check-equal?
   (entry-first-text (make-bib #:title "Title" #:doi "10.1/x" #:note ""))
   "Title. doi:10.1/x")
  (check-equal?
   (entry-first-text (make-bib #:title "Title" #:doi "10.1/x" #:note " \n "))
   "Title. doi:10.1/x")
  (check-equal?
   (entry-first-text (make-bib #:title "Title"
                               #:url "https://example.org"
                               #:note "A note"))
   "Title. https://example.org A note")
  (check-false (journal-location #f))
  (check-false (techrpt-location #:institution #f))
  (check-false (proceedings-location #f))
  (check-false (book-chapter-location #f))
  (check-equal? (content->string (proceedings-location #f #:publisher "ACM"))
                "ACM")
  (check-equal?
   (content->string
    (book-location
     #:edition "second"
     #:chapter 3
     #:series "LNCS"
     #:volume 42
     #:number 7
     #:pages '(10 20)
     #:publisher "Springer"
     #:address "Berlin"))
   "Second edition, 3, LNCS, 42(7), pp. 10--20. Springer, Berlin")

  (define multi-note
    (make-bib
     #:title "Title"
     #:note "First paragraph.\n\nSecond paragraph."))
  (define entry
    (bib->entry multi-note author+date-style #f
                default-render-date-bib 1))
  (check-true (compound-paragraph? entry))
  (define paragraphs (compound-paragraph-blocks entry))
  (check-equal? (length paragraphs) 2)
  (check-equal?
   (content->string (paragraph-content (first paragraphs)))
   "Title. First paragraph.")
  (check-equal?
   (content->string (paragraph-content (second paragraphs)))
   "Second paragraph."))

(define (proceedings-location
         #:editor [editor_ #f]
         location
         #:series [series #f]
         #:volume [volume #f]
         #:number [number #f]
         #:pages [pages #f]
         #:organization [organization #f]
         #:publisher [publisher #f]
         #:address [address #f])
  (define details
    (concatenate-content
     #:separator ", "
     (and editor_ (editor editor_))
     (and location @italic{Proc. @contentify[location]})
     (series-volume-number-pages-content series volume number pages)))
  (concatenate-content
   (and details
        (if location
            (concatenate-content "In " details)
            details))
   #:separator ". "
   (organization-publisher-address-content organization publisher address)))

(define (journal-location
         location
         #:volume [volume #f]
         #:number [number #f]
         #:pages [pages #f])
  (concatenate-content
   (and location @italic{@contentify[location]})
   #:separator " "
   (series-volume-number-pages-content #f volume number pages)))

;; The URL is now redundant with the URL in make-bib, so we now (2025-12) make it optional
(define (webpage-location (url #f) #:accessed [accessed #f])
  (concatenate-content
   (and url ((url-rendering) url))
   #:separator " "
   (and accessed @list{(accessed @contentify[accessed])})))

(define (capitalize-string s)
  (string-append
   (string (char-upcase (string-ref s 0)))
   (substring s 1)))

(define (capitalize-content content)
  (match (flatten-content content)
    [(? non-empty-string? s) (capitalize-string s)]
    [(cons (? non-empty-string? s) r) (cons (capitalize-string s) r)]
    [x x]))

(define (flatten-content content)
  (define a '())
  (let loop ((c content))
    (cond
      [(pair? c)                                       (loop (car c)) (loop (cdr c))]
      [(or (null? c) (not c) (void? c) (equal? c ""))  (void)]
      [else                                            (set! a (cons (contentify c) a))]))
  (match a
    ['() #f]
    [(list x) x]
    [else (reverse a)]))

(define (concatenate-content #:separator (separator #f) . content)
  (define l (filter values (map flatten-content content)))
  (and (pair? l) (flatten-content (if separator (add-between l separator) l))))

(define (book-location
         #:edition [edition #f]
         #:chapter [chapter #f]
         #:editor [editor_ #f]
         #:series [series #f]
         #:volume [volume #f]
         #:number [number #f]
         #:pages [pages #f]
         #:publisher [publisher #f]
         #:address [address #f])
  (concatenate-content
   (concatenate-content
    #:separator ", "
    (edition-content edition)
    chapter
    (and editor_ (editor editor_))
    (series-volume-number-pages-content series volume number pages))
   #:separator ". "
   (organization-publisher-address-content #f publisher address)))

(define (booklet-location
         #:howpublished [howpublished #f]
         #:address [address #f])
  (concatenate-content #:separator ". "
    howpublished
    address))

(define (misc-location
         #:howpublished [howpublished #f])
  (and howpublished (contentify howpublished)))

(define (manual-location
         #:organization [organization #f]
         #:edition [edition #f])
  (concatenate-content
   (edition-content edition)
   #:separator ", "
   organization))

(define (techrpt-location
         #:institution institution
         #:type [type #f]
         #:number [number #f]
         #:address [address #f])
  (concatenate-content #:separator ", "
    institution type number address))

(define (dissertation-location
         #:institution institution
         #:degree [degree "PhD"]
         #:type [type #f]
         #:address [address #f])
  (concatenate-content #:separator ", "
    @list{@contentify[degree] dissertation}
    institution
    type
    address))

(define (book-chapter-location
         location
         #:edition [edition #f]
         #:chapter [chapter #f]
         #:editor [editor_ #f]
         #:series [series #f]
         #:volume [volume #f]
         #:number [number #f]
         #:pages [pages #f]
         #:publisher [publisher #f]
         #:address [address #f])
  (concatenate-content #:separator " "
   (and location @list{In @italic{@contentify[location]}})
   (book-location #:edition edition #:chapter chapter #:editor editor_
         #:series series #:volume volume #:number number #:pages pages
         #:publisher publisher #:address address)))

;; ----------------------------------------

(define (author-name first last #:suffix [suffix #f])
  (define first* (contentify first))
  (define last* (contentify last))
  (define suffix* (contentify suffix))

  ;; Plain-text projections are needed for sorting.
  (define first-string (stringify first*))
  (define last-string (stringify last*))
  (define suffix-string (stringify suffix*))
  (make-author-element
   #f
   (concatenate-content #:separator " "
    (if (abbreviate-given-names)
              (given-names->initials first-string)
              first*)
    last*
    suffix*)
   (format "~a ~a~a" last-string first-string
           (if suffix-string (format " ~a" suffix-string) ""))
   last*))

(define (org-author-name org)
  (make-author-element
   #f
   (list org)
   (content->string org)
   org))

(define (other-authors)
  (make-other-author-element
   #f
   (list "Alia")
   "al."
   (list "al" ._)))

(define (authors name . names*)
  (define names (map parse-author (cons name names*)))
  (define slash-names (string-join (map author-element-names names) " / "))
  (define cite
    (case (length names)
      [(1) (author-element-cite (car names))]
      [(2) (if (other-author-element? (cadr names))
               (list (author-element-cite (car names)) " et al" @._)
               (list
                (author-element-cite (car names))
                " and "
                (author-element-cite (cadr names))))]
      [else (list (author-element-cite (car names)) " et al" ._)]))
  (make-author-element
     #f
     (let loop ([names names] [prefix 0])
       (cond [(null? (cdr names))
              (case prefix
                [(0) names]
                [(1) (if (other-author-element? (car names))
                         (list " et al" ._)
                         (list " and " (car names)))]
                [else (if (other-author-element? (car names))
                          (list ", et al" ._)
                          (list ", and " (car names)))])]
             [else
              (case prefix
                [(0) (list* (car names)
                            (loop (cdr names) (add1 prefix)))]
                [else (list* ", "
                             (car names)
                             (loop (cdr names) (add1 prefix)))])]))
     slash-names
     cite))

(define (editor name)
  (let ([name (parse-author name)])
    (make-author-element
     #f
     (append (element-content name)
             '(" (Ed.)"))
     (author-element-names name)
     (author-element-cite name))))

(define (edition-content edition)
  (and edition
       @list{@(capitalize-content edition) edition}))
(define (pages-content pages)
  (and pages @elem{pp. @(contentify (car pages))--@(contentify (cadr pages))}))
(define (series-volume-number-pages-content series volume number pages)
  (concatenate-content
   series
   #:separator ", "
   (concatenate-content
    volume
    (and number @list{(@contentify[number])}))
   (pages-content pages)))
(define (organization-publisher-address-content organization publisher address)
  (concatenate-content
   organization
   #:separator ". "
   (concatenate-content
    publisher
    #:separator ", "
    address)))

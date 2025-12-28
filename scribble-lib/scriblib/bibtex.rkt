#lang at-exp racket/base
(require racket/function
         racket/match
         racket/list
         racket/string)

;; Spec but not official: https://www.openoffice.org/bibliographic/bibtex-defs.html
;; Informal spec: https://www.bibtex.com/g/bibtex-format/
;; More incomplete spec: https://www.bibtex.org/Format/ https://www.bibtex.org/SpecialSymbols/
;; Examples for test suite: https://www.bibtex.com/e/entry-types/
;; Great resource (follow PDFs at the end): https://www.andy-roberts.net/latex/bibliographies/
;; Then, there is the much richer (but less used) biblatex: https://www.overleaf.com/learn/latex/Bibliography_management_with_biblatex

(struct bibdb (raw bibs))

(define (bibtex-parse ip)
  (define STRING-DB (make-hash))
  (define ENTRY-DB (make-hash))

  (define (perror ip sym fmt . args)
    (define loc (call-with-values (λ () (port-next-location ip)) list))
    (apply error sym (string-append fmt " @ line ~a column ~a byte ~a") (append args loc)))

  (define (read-while pred ip)
    (list->string
     (let loop ()
       (match (peek-char ip)
         [(and (? char?) (? pred))
          (cons (read-char ip)
                (loop))]
         [_
          empty]))))

  (define (read-until pred ip)
    (read-while (negate pred) ip))

  (define (slurp-whitespace ip)
    (read-while (λ (c) (and (char? c) (char-whitespace? c))) ip))

  (define (read-entries ip)
    (slurp-whitespace ip)
    (match (read-char ip)
      [#\%
       (read-line ip)
       (read-entries ip)]
      [#\@
       (read-entry ip)
       (read-entries ip)]
      [(? eof-object?)
       (void)]
      [c
       ;; All other characters are comments.
       (read-entries ip)]))

  (define (read-entry ip)
    (match (read-until (λ (c) (or (char=? c #\{)
                                  (char=? c #\()))
                       ip)
      [(app string-foldcase "string")
       (slurp-whitespace ip)
       (match (read-char ip)
         [#\{
          (void)]
         [c
          (perror ip 'read-entry "Parsing entry, expected {, got ~v" c)])
       (define tag (read-tag ip))
       (slurp-whitespace ip)
       (match (read-char ip)
         [#\=
          (slurp-whitespace ip)
          (define string (read-value ip))
          (slurp-whitespace ip)
          (match (read-char ip)
            [#\}
             (hash-set! STRING-DB tag string)]
            [c
             (perror ip 'read-entry "Parsing string, expected }, got ~v; tag is ~v; string is ~v" c tag string)])]
         [c
          (perror ip 'read-entry "Parsing string, expected =, got ~v; tag is ~v" c tag)])]
      [(or (app string-foldcase "comment")
           (app string-foldcase "preamble"))
       (read-char ip)
       (let loop ()
         (read-until (λ (c) (or (char=? c #\{) (char=? c #\}))) ip)
         (match (read-char ip)
           [#\{
            (loop) (loop)]
           [#\}
            (void)]))]
      [typ
       (read-char ip)
       (slurp-whitespace ip)
       ;; TODO only accept [-_:a-zA-Z0-9]*, then slurp-whitespace before comma
       ;; “The citekey can be any combination of alphanumeric characters including the characters "-", "_", and ":".” -- https://www.bibtex.com/g/bibtex-format/
       (define label (string-foldcase (read-until (λ (c) (char=? c #\,)) ip)))
       (read-char ip)
       (define alist
         (let loop ()
           (slurp-whitespace ip)
           (define atag (read-tag ip))
           (cond
             [(string=? "" atag)
              (read-char ip)
              (hash)]
             [else
              (slurp-whitespace ip)
              (match (read-char ip)
                [#\=
                 (slurp-whitespace ip)
                 (define aval (read-value ip))
                 (slurp-whitespace ip)
                 (match (read-char ip)
                   [#\,
                    (hash-set (loop) atag aval)]
                   [#\}
                    (hash atag aval)]
                   [c
                    (perror ip 'read-entry "Parsing entry, expected , or }, got ~v; label is ~v; atag is ~v; aval is ~v" c label atag aval)])]
                [c
                 (perror ip 'read-entry "Parsing entry tag, expected =, got ~v; label is ~v; atag is ~v" c label atag)])])))
       (hash-set! ENTRY-DB label
                  (hash-set alist 'type (string-foldcase typ)))]))

  (define (read-tag ip)
    (slurp-whitespace ip)
    (string-foldcase
     (read-until
      (λ (c) (or (char-whitespace? c)
                 (char=? c #\=)
                 (char=? c #\{)
                 (char=? c #\})))
      ip)))

  (define (read-braced-value ip)
    (read-char ip)
    (let loop ()
      (define first-part (read-until (λ (c) (or (char=? c #\{) (char=? c #\})))
                                     ip))
      (match (peek-char ip)
        [#\{
         (string-append first-part (read-value ip) (loop))]
        [#\}
         (read-char ip)
         first-part])))

  (define (read-value ip)
    (slurp-whitespace ip)
    (define first-part (read-value-single ip))
    (slurp-whitespace ip)
    (match (peek-char ip)
      [#\#
       (read-char ip)
       (string-append first-part (read-value ip))]
      [_
       first-part]))

  (define (read-value-single ip)
    (slurp-whitespace ip)
    (match (peek-char ip)
      [#\{
       (read-braced-value ip)]
      [#\"
       (read-char ip)
       (let loop ()
         (define first-part (read-until (λ (c) (or (char=? c #\{) (char=? c #\")))
                                        ip))
         (match (peek-char ip)
           [#\{
            (string-append first-part (read-braced-value ip) (loop))]
           [#\"
            (read-char ip)
            first-part]))]
      [(? char-numeric?)
       (read-while char-numeric? ip)]
      [(? char-alphabetic?)
       (define string-tag (read-until (λ (c) (or (char-whitespace? c)
                                                 (char=? c #\,)))
                                      ip))
       (hash-ref STRING-DB string-tag
                 (λ () string-tag))]
      [c
       (perror ip 'read-value "Parsing value, expected {, got ~v" c)]))

  (read-entries ip)

  (bibdb ENTRY-DB (make-hash)))

(define (path->bibdb pth)
  (define bibdb
    (with-input-from-file
        pth
      (λ ()
        (port-count-lines! (current-input-port))
        (bibtex-parse (current-input-port)))))
  bibdb)

(require scriblib/autobib
         scribble/manual)

(define-syntax-rule
  (define-bibtex-cite bib-pth
    ~cite-id citet-id generate-bibliography-id . options)
  (begin
    (define-cite autobib-cite autobib-citet generate-bibliography-id . options)
    (define-bibtex-cite* bib-pth
      autobib-cite autobib-citet
      ~cite-id citet-id)))

(define ((make-citer bibtex-db citer) f . r)
  (apply citer
         (filter-map
          (λ (key)
            (and (not (string=? "\n" key))
                 (generate-bib bibtex-db key)))
          (append-map (curry regexp-split #px"\\s+")
                      (cons f r)))))

(define-syntax-rule
  (define-bibtex-cite* bib-pth
    autobib-cite autobib-citet
    ~cite-id citet-id)
  (begin
    (define bibtex-db (path->bibdb bib-pth))
    (define ~cite-id (make-citer bibtex-db autobib-cite))
    (define citet-id (make-citer bibtex-db autobib-citet))))

;; Seems a little redundant to convert latex escapes into unicode only to
;; convert them back into latex, but we need to sort authors so we can't
;; leave them as literal-chars.
(define (latex-to-unicode str)
  ; This is probably defined somewhere...
  ; NOTE: Incomplete. Please file PR if you need more.
  (define converts
    '(("\\'\\i" . "ı́")
      ("\\\"u" . "ü")
      ("\\\"o" . "ö")
      ("\\\"i" . "ï")
      ("\\'i" . "í")
      ("\\i" . "ı")
      ("\\'a" . "á")
      ("\\'A" . "Á")
      ("\\~a" . "ã")
      ("\\`a" . "À")
      ("\\~A" . "Ã")))
  (for/fold ([str str])
            ([p converts])
    (string-replace str (car p) (cdr p))))

(define (parse-author as)
  (and as
      (apply authors
         (for/list ([a (in-list (regexp-split #px"\\s+and\\s+" as))])
           (define (trim s)
             (string-trim (regexp-replace #px"\\s+" s " ")))
           (match (latex-to-unicode a)
             [(pregexp #px"^(.*),(.*),(.*)$" (list _ two suffix one))
              (author-name (trim one) (trim two) #:suffix (trim suffix))]
             [(pregexp #px"^(.*),(.*)$" (list _ two one))
              (author-name (string-trim one) (string-trim two))]
             [(pregexp #px"^(.*?)\\s+(\\p{Ll}[^\\s]*(\\s+\\p{Ll}[^\\s]*)*)\\s+(.*)$" (list _ one von-like _ two))
              (author-name (string-trim one)
                           (string-append (string-trim von-like) " " (string-trim two)))]
             [space-separated
              (match (regexp-split #px"\\s+" space-separated)
                [(list one) (org-author-name one)]
                [(list one two) (author-name one two)]
                [(list-rest first rest)
                 (author-name (apply string-append (add-between (cons first (drop-right rest 1))
                                                                " "))
                              (last rest))])])))))

(module+ test
  (require rackunit)

  ;; use this as a predicate to hack around lack of
  ;; ability to use equal? on author element structs;
  ;; unfortunately, it ony compares the composed strings
  (define (print-as-equal-string? a b)
    (equal? (format "~s" a)
            (format "~s" b)))

  (check
   print-as-equal-string?
   (parse-author "James Earl Jones")
   (authors
    (author-name "James Earl" "Jones")))

  (check
   print-as-equal-string?
   (parse-author "Tim Robbins and Morgan Freeman")
   (authors (author-name "Tim" "Robbins")
            (author-name "Morgan" "Freeman")))

  (check
   print-as-equal-string?
   (parse-author "Edward L. Deci and Robert J. Vallerand and Luc G. Pelletier and Richard M. Ryan")
   (authors (author-name "Edward L." "Deci")
            (author-name "Robert J." "Vallerand")
            (author-name "Luc G." "Pelletier")
            (author-name "Richard M." "Ryan")))

  (check
   print-as-equal-string?
   (parse-author "Lst, Fst")
   (authors
    (author-name "Fst" "Lst")))

  (check
   print-as-equal-string?
   (parse-author "Lst,Fst")
   (authors
    (author-name "Fst" "Lst")))

  (check
   print-as-equal-string?
   (parse-author "James, Earl Jones")
   (authors
    (author-name "Earl Jones" "James")))

  (check
   print-as-equal-string?
   (parse-author "James,Earl Jones")
   (authors
    (author-name "Earl Jones" "James")))

  (check
   print-as-equal-string?
   (parse-author "LstA LstB, Fst")
   (authors
    (author-name "Fst" "LstA LstB")))

  (check
   print-as-equal-string?
   (parse-author "LstA LstB,Fst")
   (authors
    (author-name "Fst" "LstA LstB")))

  (check
   print-as-equal-string?
   (parse-author "LstA LstB, FstA FstB")
   (authors
    (author-name "FstA FstB" "LstA LstB")))

  (check
   print-as-equal-string?
   (parse-author "LstA LstB,FstA FstB")
   (authors
    (author-name "FstA FstB" "LstA LstB")))

  (check
   print-as-equal-string?
   (parse-author "James, Jr, Earl Jones")
   (authors
    (author-name "Earl Jones" "James" #:suffix "Jr")))

  (check
   print-as-equal-string?
   (parse-author "James,Jr, Earl Jones")
   (authors
    (author-name "Earl Jones" "James" #:suffix "Jr")))

  (check
   print-as-equal-string?
   (parse-author "James, Jr,Earl Jones")
   (authors
    (author-name "Earl Jones" "James" #:suffix "Jr")))

  (check
   print-as-equal-string?
   (parse-author "James,Jr,Earl Jones")
   (authors
    (author-name "Earl Jones" "James" #:suffix "Jr")))

  (check
   print-as-equal-string?
   (parse-author "James, III, Earl Jones")
   (authors
    (author-name "Earl Jones" "James" #:suffix "III")))

  (check
   print-as-equal-string?
   (parse-author "James,III, Earl Jones")
   (authors
    (author-name "Earl Jones" "James" #:suffix "III")))

  (check
   print-as-equal-string?
   (parse-author "James, III,Earl Jones")
   (authors
    (author-name "Earl Jones" "James" #:suffix "III")))

  (check
   print-as-equal-string?
   (parse-author "James,III,Earl Jones")
   (authors
    (author-name "Earl Jones" "James" #:suffix "III")))

  (check
   print-as-equal-string?
   (parse-author "James Jack von Earl Jones")
   (authors
    (author-name "James Jack" "von Earl Jones")))

  (check
   print-as-equal-string?
   (parse-author "James Jack de la Earl Jones")
   (authors
    (author-name "James Jack" "de la Earl Jones")))

  (check
   print-as-equal-string?
   (parse-author "James Jack van der Earl Jones")
   (authors
    (author-name "James Jack" "van der Earl Jones")))

  (check
   print-as-equal-string?
   (parse-author "James Jack von de la Earl Jones")
   (authors
    (author-name "James Jack" "von de la Earl Jones")))

  (check
   print-as-equal-string?
   (parse-author "James Jack di Earl Jones")
   (authors
    (author-name "James Jack" "di Earl Jones")))

  (check
   print-as-equal-string?
   (parse-author "First fOn bER Last")
   (authors
    (author-name "First" "fOn bER Last")))

  (check
   print-as-equal-string?
   (parse-author "Deci, Edward L. and Robert J. Vallerand and Pelletier, Luc G. and Ryan, Jr, Richard M.")
   (authors (author-name "Edward L." "Deci")
            (author-name "Robert J." "Vallerand")
            (author-name "Luc G." "Pelletier")
            (author-name "Richard M." "Ryan" #:suffix "Jr")))

  (check
   print-as-equal-string?
   (parse-author "Foo anderson") ;; Should not be parsed as the two authors "Foo" & "erson"
   (authors
    (author-name "Foo" "anderson"))))

(define (parse-pages ps)
  (match ps
    [(regexp #rx"^([0-9]+)[-—–]+([0-9]+)$" (list _ f l)) ;; NB: mind the Unicode dashes
     (list f l)]
    [#f
     #f]
    [_
     (error 'parse-pages "Invalid page format ~e" ps)]))

(require scribble/core)
(define (support-escapes s)
  (elem #:style (make-style #f '(exact-chars)) s))

(define (generate-bib db key)
  (match-define (bibdb raw bibs) db)
  (hash-ref! bibs (string-foldcase key)
             (λ ()
               (define the-raw (hash-ref raw (string-foldcase key) (λ () (error 'bibtex "Unknown citation ~e" key))))
               (define (raw-attr a [def #f])
                 (hash-ref the-raw a def))
               (define (raw-attr* a)
                 (hash-ref the-raw a
                           (λ () (error 'bibtex "Key ~a is missing attribute ~a, has ~a"
                                        key a the-raw))))
               (match (raw-attr 'type)
                 ;; TODO: eid replaces pages for online journals
                 ;; TODO: add isbn for books (inbooks, proceedings, inproceedings?)
                 ;; TODO: add issn for periodicals (?)
                 ;; TODO: add optional urldate everywhere?
                 ;; TODO: add keywords everywhere as in biblatex?
                 ["article" ;; An article from a journal or magazine.
                  (make-bib
                        #:type 'article
                        ;; required:
                        #:author (parse-author (raw-attr "author"))
                        #:title (support-escapes (raw-attr "title"))
                        #:date (raw-attr "year") ;; TODO: optional month
                        #:location (journal-location
                                      (raw-attr* "journal")
                                      ;; optional:
                                      #:pages (parse-pages (raw-attr "pages"))
                                      #:number (raw-attr "number")
                                      #:volume (raw-attr "volume"))
                        ;; optional:
                        #:note (raw-attr "note")
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:url (raw-attr "url")
                        #:doi (raw-attr "doi"))]
                 ["book" ;; A book with an explicit publisher.
                  (make-bib
                        #:type 'book
                        #:is-book? #t
                        ;; required:
                        #:author (parse-author (raw-attr "author")) ;; author OR editor is required
                        #:title (support-escapes (raw-attr "title"))
                        #:date (raw-attr "year") ;; TODO: optional month
                        #:location (book-location
                                      #:publisher (raw-attr "publisher")
                                      ;; optional:
                                      #:editor (parse-author (raw-attr "editor")) ;; see above
                                      #:volume (raw-attr "volume") ;; volume OR number
                                      #:number (raw-attr "number")
                                      #:series (raw-attr "series")
                                      #:address (raw-attr "address")
                                      #:edition (raw-attr "edition"))
                        ;; optional:
                        #:note (raw-attr "note")
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:url (raw-attr "url")
                        #:doi (raw-attr "doi"))]
                 ["booklet" ;; A work that is printed and bound, but without a named publisher or sponsoring institution.
                  (make-bib
                        #:type 'booklet
                        #:is-book? #t ;; TODO or #f??? or have make-bib accept a #:type ???
                        ;; required:
                        #:title (support-escapes (raw-attr "title"))
                        ;; optional:
                        #:author (parse-author (raw-attr "author")) ;; TODO: make it optional
                        #:date (raw-attr "year") ;; TODO: month
                        #:location (booklet-location
                                      #:howpublished (raw-attr "howpublished")
                                      #:address (raw-attr "address"))
                        #:note (raw-attr "note")
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:url (raw-attr "url")
                        #:doi (raw-attr "doi"))]
                 [(or "conference" ;; The same as INPROCEEDINGS, included for Scribe compatibility.
                      "inproceedings") ;; An article in a conference proceedings.
                  (make-bib
                        #:type 'inproceedings
                        ;; required:
                        #:author (parse-author (raw-attr "author"))
                        #:title (support-escapes (raw-attr "title"))
                        #:date (raw-attr "year") ;; TODO: optional month
                        #:location (proceedings-location
                                      (raw-attr "booktitle")
                                      ;; optional:
                                      #:editor (parse-author (raw-attr "editor"))
                                      #:series (raw-attr "series")
                                      #:volume (raw-attr "volume") ;; volume OR number
                                      #:number (raw-attr "number")
                                      #:pages (parse-pages (raw-attr "pages"))
                                      #:address (raw-attr "address")
                                      #:organization (raw-attr "organization")
                                      #:publisher (raw-attr "publisher"))
                        ;; optional:
                        #:note (raw-attr "note")
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:url (raw-attr "url")
                        #:doi (raw-attr "doi"))]
                 ["inbook" ;; A part of a book, which may be a chapter (or section or whatever) and/or a range of pages.
                  (make-bib
                        #:type 'inbook
                        #:is-book? #t ;; TODO or #f ???
                        ;; required:
                        #:author (parse-author (raw-attr "author")) ;; author OR editor is required
                        #:title (support-escapes (raw-attr "title"))
                        #:date (raw-attr "year") ;; TODO: optional month
                        #:location (book-chapter-location
                                      #:editor (parse-author (raw-attr "editor")) ;; see above
                                      #:chapter (raw-attr "chapter") ;; chapter OR pages is required
                                      #:pages (raw-attr "pages")
                                      #:publisher (raw-attr "publisher")
                                      ;; optional:
                                      #:volume (raw-attr "volume") ;; volume OR number
                                      #:number (raw-attr "number")
                                      #:series (raw-attr "series")
                                      #:address (raw-attr "address")
                                      #:edition (raw-attr "edition"))
                        ;; optional:
                        #:note (raw-attr "note")
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:url (raw-attr "url")
                        #:doi (raw-attr "doi"))]
                 ["incollection" ;; A part of a book having its own title.
                  ;; TODO: figure out why https://www.openoffice.org/bibliographic/bibtex-defs.html
                  ;; talks about a "type" kind of label, what it does, who uses it for what...
                  ;; or whether it's a bug in that page.
                  (make-bib
                        #:type 'incollection
                        ;; required:
                        #:author (parse-author (raw-attr "author"))
                        #:title (support-escapes (raw-attr "title"))
                        #:date (raw-attr "year") ;; TODO: optional month
                        #:location (book-chapter-location
                                      (raw-attr "booktitle")
                                      #:publisher (raw-attr "publisher")
                                      ;; optional:
                                      #:editor (parse-author (raw-attr "editor"))
                                      #:volume (raw-attr "volume") ;; volume OR number
                                      #:number (raw-attr "number")
                                      #:series (raw-attr "series")
                                      #:chapter (raw-attr "chapter")
                                      #:pages (raw-attr "pages")
                                      #:address (raw-attr "address")
                                      #:edition (raw-attr "edition"))
                        ;; optional:
                        #:note (raw-attr "note")
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:url (raw-attr "url")
                        #:doi (raw-attr "doi"))]
                 ["manual" ;; Technical documentation
                  (make-bib
                        #:type 'manual
                        ;; required:
                        #:title (support-escapes (raw-attr "title"))
                        ;; optional:
                        #:author (parse-author (raw-attr "author"))
                        #:date (raw-attr "year") ;; TODO: optional month
                        #:location (manual-location
                                      ;; optional:
                                      #:organization (raw-attr "organization")
                                      #:edition (raw-attr "edition"))
                        #:note (raw-attr "note")
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:url (raw-attr "url")
                        #:doi (raw-attr "doi"))]
                 ["mastersthesis" ;; A Master's thesis.
                  (make-bib
                        #:type 'mastersthesis
                        ;; required:
                        #:author (parse-author (raw-attr "author"))
                        #:title (support-escapes (raw-attr "title"))
                        #:date (raw-attr "year") ;; TODO: optional month
                        #:location (dissertation-location
                                      #:institution (raw-attr "school")
                                      #:degree "Master’s"
                                      ;; optional:
                                      #:type (raw-attr "type")
                                      #:address (raw-attr "address"))
                        ;; optional:
                        #:note (raw-attr "note")
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:url (raw-attr "url")
                        #:doi (raw-attr "doi"))]
                 ["misc" ;; Use this type when nothing else fits.
                  (make-bib
                        #:type 'misc
                        ;; optional: (no required field)
                        #:author (parse-author (raw-attr "author"))
                        #:title (support-escapes (raw-attr "title"))
                        #:date (raw-attr "year") ;; TODO: month
                        #:location (misc-location
                                      #:howpublished (raw-attr "howpublished"))
                        ;; optional:
                        #:note (raw-attr "note")
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:url (raw-attr "url")
                        #:doi (raw-attr "doi"))]
                 ["phdthesis" ;; A PhD thesis.
                  (make-bib
                        #:type 'phdthesis
                        ;; required:
                        #:author (parse-author (raw-attr "author"))
                        #:title (support-escapes (raw-attr "title"))
                        #:date (raw-attr "year") ;; TODO: optional month
                        #:location (dissertation-location
                                      #:institution (raw-attr "school")
                                      #:degree "PhD"
                                      ;; optional:
                                      #:type (raw-attr "type")
                                      #:address (raw-attr "address"))
                        ;; optional:
                        #:note (raw-attr "note")
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:url (raw-attr "url")
                        #:doi (raw-attr "doi"))]
                 ["proceedings" ;; The proceedings of a conference.
                  (make-bib
                        #:type 'proceedings
                        ;; required:
                        #:title (support-escapes (raw-attr "title"))
                        #:date (raw-attr "year") ;; TODO: optional month
                        ;; optional:
                        #:location (proceedings-location
                                      (raw-attr "booktitle")
                                      ;; optional:
                                      #:editor (parse-author (raw-attr "editor"))
                                      #:volume (raw-attr "volume") ;; volume OR number
                                      #:number (raw-attr "number")
                                      #:series (raw-attr "series")
                                      #:address (raw-attr "address")
                                      #:organization (raw-attr "organization")
                                      #:publisher (raw-attr "publisher"))
                        #:note (raw-attr "note")
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:url (raw-attr "url")
                        #:doi (raw-attr "doi"))]
                 ["techreport" ;; A report published by a school or other institution, usually numbered within a series.
                 ;; Required fields: author, title, institution, year. Optional fields: type, number, address, month, note.
                  (make-bib
                        #:type 'techreport
                        ;; required:
                        #:author (parse-author (raw-attr "author"))
                        #:title (support-escapes (raw-attr "title"))
                        #:date (raw-attr "year") ;; TODO: optional month
                        #:location (techrpt-location
                                      #:institution (raw-attr "institution")
                                      ;; optional:
                                      #:type (raw-attr "type")
                                      #:number (raw-attr "number")
                                      #:address (raw-attr "address"))
                        ;; optional:
                        #:note (raw-attr "note")
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:url (raw-attr "url")
                        #:doi (raw-attr "doi"))]
                 ["unpublished" ;; A document having an author and title, but not formally published.
                  (make-bib
                        #:type 'unpublished
                        ;; required:
                        #:author (parse-author (raw-attr "author"))
                        #:title (support-escapes (raw-attr "title"))
                        #:note (raw-attr "note")
                        ;; optional:
                        #:date (raw-attr "year") ;; TODO: month
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:url (raw-attr "url")
                        #:doi (raw-attr "doi"))]
                 ;; SEEN IN THE WILD, BUT WHERE ARE THESE SPECIFIED???
                 ["online"
                  (make-bib
                        #:type 'webpage
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:title (support-escapes (raw-attr "title"))
                        #:url (raw-attr "url")
                        #:location (webpage-location
                                     #:accessed (raw-attr "urldate")) ;; when visited
                        #:author (parse-author (raw-attr "author"))
                        #:note (raw-attr "note")
                        #:date (raw-attr "year") ;; TODO: month ;; presumably when written
                        #:doi (raw-attr "doi"))]
                 ["webpage"
                  (make-bib
                        #:type 'webpage
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:title (support-escapes (raw-attr "title"))
                        #:url (raw-attr "url")
                        #:location (webpage-location
                                     #:accessed (raw-attr "lastchecked"))
                        #:author (parse-author (raw-attr "author"))
                        #:note (raw-attr "note")
                        #:date (raw-attr "year") ;; TODO: month ;; presumably when written
                        #:doi (raw-attr "doi"))]
                 [_
                  (make-bib #:title (format "~v" the-raw))]))))

(provide (struct-out bibdb)
         path->bibdb
         bibtex-parse
         define-bibtex-cite
         define-bibtex-cite*)

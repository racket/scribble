#lang at-exp racket/base
(require racket/function
         racket/match
         racket/list
         racket/string
         scriblib/autobib
         scribble/core
         scribble/manual
         "private/read-latex.rkt")

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
    (read-while char-whitespace? ip))

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

  (define (read-delimited-value ip terminator)
    (define out (open-output-string))
    (let loop ([depth 0])
      (match (read-char ip)
        [(? eof-object?)
         (perror ip 'read-value "Unexpected EOF in delimited value")]
        [#\\
         (write-char #\\ out)
         (define next (read-char ip))
         (when (eof-object? next)
           (perror ip 'read-value "Unexpected EOF after backslash"))
         (write-char next out)
         (loop depth)]
        [#\{
         (write-char #\{ out)
         (loop (add1 depth))]
        [#\}
         (cond
           [(positive? depth)
            (write-char #\} out)
            (loop (sub1 depth))]
           [(eqv? terminator #\}) (get-output-string out)]
           [else (perror ip 'read-value "Unexpected closing brace")])]
        [#\"
         (if (and (zero? depth) (eqv? terminator #\"))
             (get-output-string out)
             (begin (write-char #\" out) (loop depth)))]
        [c
         (write-char c out)
         (loop depth)])))

  (define (read-braced-value ip)
    (read-char ip)
    (read-delimited-value ip #\}))

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
       (read-delimited-value ip #\")]
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

(struct name-word (content) #:transparent)

(define (name-tokens content)
  (define tokens null)
  (define word-parts null)
  (define text (open-output-string))

  (define (flush-text!)
    (define s (get-output-string text))
    (unless (string=? s "")
      (set! word-parts (cons s word-parts)))
    (set! text (open-output-string)))

  (define (flush-word!)
    (flush-text!)
    (when (pair? word-parts)
      (define parts (reverse word-parts))
      (set! tokens
            (cons (name-word (if (null? (cdr parts)) (car parts) parts))
                  tokens))
      (set! word-parts null)))

  (define (delimiter! token)
    (flush-word!)
    (unless (and (eq? token 'space)
                 (pair? tokens)
                 (eq? (car tokens) 'space))
      (set! tokens (cons token tokens))))

  (for ([part (in-list (if (list? content) content (list content)))])
    (cond
      [(string? part)
       (for ([c (in-string part)])
         (cond [(char-whitespace? c) (delimiter! 'space)]
               [(char=? c #\,) (delimiter! 'comma)]
               [else (write-char c text)]))]
      [else
       (flush-text!)
       (set! word-parts (cons part word-parts))]))
  (flush-word!)
  (reverse tokens))

(define (trim-name-tokens tokens)
  (define (space? token) (eq? token 'space))
  (reverse (dropf (reverse (dropf tokens space?)) space?)))

(define (split-name-tokens tokens delimiter?)
  (let loop ([tokens tokens] [part null] [parts null])
    (cond
      [(null? tokens)
       (reverse (cons (trim-name-tokens (reverse part)) parts))]
      [(delimiter? (car tokens))
       (loop (cdr tokens) null
             (cons (trim-name-tokens (reverse part)) parts))]
      [else (loop (cdr tokens) (cons (car tokens) part) parts)])))

(define (and-word? token)
  (and (name-word? token)
       (string? (name-word-content token))
       (string=? (name-word-content token) "and")))

(define (split-authors tokens)
  (let loop ([tokens tokens] [part null] [parts null])
    (match tokens
      ['() (reverse (cons (trim-name-tokens (reverse part)) parts))]
      [(list* 'space (? and-word?) 'space rest)
       (loop rest null (cons (trim-name-tokens (reverse part)) parts))]
      [(cons first rest) (loop rest (cons first part) parts)])))

(define (join-name-words words)
  (match words
    ['() ""]
    [(list one) one]
    [_ (if (andmap string? words)
           (string-join words " ")
           (add-between words " "))]))

(define (lowercase-initial? word)
  (define s (content->string word))
  (for/first ([c (in-string s)] #:when (char-alphabetic? c))
    (char-lower-case? c)))

(define (parse-one-author tokens)
  (define parts
    (for/list ([part (in-list (split-name-tokens tokens
                                              (λ (token) (eq? token 'comma))))])
      (for/list ([token (in-list part)] #:when (name-word? token))
        (name-word-content token))))
  (match parts
    [(list (list name)) (org-author-name name)]
    [(list (list)) (error 'parse-author "empty BibTeX author")]
    [(list words)
     (define von-index
       (for/first ([word (in-list (drop-right words 1))]
                   [i (in-naturals)]
                   #:when (lowercase-initial? word))
         i))
     (if von-index
         (author-name (join-name-words (take words von-index))
                      (join-name-words (drop words von-index)))
         (author-name (join-name-words (drop-right words 1))
                      (last words)))]
    [(list last first)
     (author-name (join-name-words first) (join-name-words last))]
    [(list last suffix first)
     (author-name (join-name-words first)
                  (join-name-words last)
                  #:suffix (join-name-words suffix))]
    [_ (error 'parse-author "invalid BibTeX name ~e" parts)]))

(define (parse-author as)
  (and as
       (apply authors
              (for/list ([tokens (in-list
                                  (split-authors (name-tokens (latex->content as))))])
                (parse-one-author tokens)))))

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
               (define (content-attr a [def #f])
                 (latex->content (hash-ref the-raw a def)))
               (define (content-attr* a)
                 (latex->content
                  (hash-ref the-raw a
                            (λ () (error 'bibtex "Key ~a is missing attribute ~a, has ~a"
                                         key a the-raw)))))
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
                        #:title (content-attr "title")
                        #:date (raw-attr "year") ;; TODO: optional month
                        #:location (journal-location
                                      (content-attr* "journal")
                                      ;; optional:
                                      #:pages (parse-pages (raw-attr "pages"))
                                      #:number (content-attr "number")
                                      #:volume (content-attr "volume"))
                        ;; optional:
                        #:note (content-attr "note")
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:url (raw-attr "url")
                        #:doi (raw-attr "doi"))]
                 ["book" ;; A book with an explicit publisher.
                  (make-bib
                        #:type 'book
                        #:is-book? #t
                        ;; required:
                        #:author (parse-author (raw-attr "author")) ;; author OR editor is required
                        #:title (content-attr "title")
                        #:date (raw-attr "year") ;; TODO: optional month
                        #:location (book-location
                                      #:publisher (content-attr "publisher")
                                      ;; optional:
                                      #:editor (parse-author (raw-attr "editor")) ;; see above
                                      #:volume (content-attr "volume") ;; volume OR number
                                      #:number (content-attr "number")
                                      #:series (content-attr "series")
                                      #:address (content-attr "address")
                                      #:edition (content-attr "edition"))
                        ;; optional:
                        #:note (content-attr "note")
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:url (raw-attr "url")
                        #:doi (raw-attr "doi"))]
                 ["booklet" ;; A work that is printed and bound, but without a named publisher or sponsoring institution.
                  (make-bib
                        #:type 'booklet
                        #:is-book? #t ;; TODO or #f??? or have make-bib accept a #:type ???
                        ;; required:
                        #:title (content-attr "title")
                        ;; optional:
                        #:author (parse-author (raw-attr "author")) ;; TODO: make it optional
                        #:date (raw-attr "year") ;; TODO: month
                        #:location (booklet-location
                                      #:howpublished (content-attr "howpublished")
                                      #:address (content-attr "address"))
                        #:note (content-attr "note")
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:url (raw-attr "url")
                        #:doi (raw-attr "doi"))]
                 [(or "conference" ;; The same as INPROCEEDINGS, included for Scribe compatibility.
                      "inproceedings") ;; An article in a conference proceedings.
                  (make-bib
                        #:type 'inproceedings
                        ;; required:
                        #:author (parse-author (raw-attr "author"))
                        #:title (content-attr "title")
                        #:date (raw-attr "year") ;; TODO: optional month
                        #:location (proceedings-location
                                      (content-attr* "booktitle")
                                      ;; optional:
                                      #:editor (parse-author (raw-attr "editor"))
                                      #:series (content-attr "series")
                                      #:volume (content-attr "volume") ;; volume OR number
                                      #:number (content-attr "number")
                                      #:pages (parse-pages (raw-attr "pages"))
                                      #:address (content-attr "address")
                                      #:organization (content-attr "organization")
                                      #:publisher (content-attr "publisher"))
                        ;; optional:
                        #:note (content-attr "note")
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:url (raw-attr "url")
                        #:doi (raw-attr "doi"))]
                 ["inbook" ;; A part of a book, which may be a chapter (or section or whatever) and/or a range of pages.
                  (make-bib
                        #:type 'inbook
                        #:is-book? #t ;; TODO or #f ???
                        ;; required:
                        #:author (parse-author (raw-attr "author")) ;; author OR editor is required
                        #:title (content-attr "title")
                        #:date (raw-attr "year") ;; TODO: optional month
                        #:location (book-chapter-location
                                      (content-attr* "booktitle")
                                      #:editor (parse-author (raw-attr "editor")) ;; see above
                                      #:chapter (content-attr "chapter") ;; chapter OR pages is required
                                      #:pages (parse-pages (raw-attr "pages"))
                                      #:publisher (content-attr "publisher")
                                      ;; optional:
                                      #:volume (content-attr "volume") ;; volume OR number
                                      #:number (content-attr "number")
                                      #:series (content-attr "series")
                                      #:address (content-attr "address")
                                      #:edition (content-attr "edition"))
                        ;; optional:
                        #:note (content-attr "note")
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
                        #:title (content-attr "title")
                        #:date (raw-attr "year") ;; TODO: optional month
                        #:location (book-chapter-location
                                      (content-attr* "booktitle")
                                      #:publisher (content-attr "publisher")
                                      ;; optional:
                                      #:editor (parse-author (raw-attr "editor"))
                                      #:volume (content-attr "volume") ;; volume OR number
                                      #:number (content-attr "number")
                                      #:series (content-attr "series")
                                      #:chapter (content-attr "chapter")
                                      #:pages (parse-pages (raw-attr "pages"))
                                      #:address (content-attr "address")
                                      #:edition (content-attr "edition"))
                        ;; optional:
                        #:note (content-attr "note")
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:url (raw-attr "url")
                        #:doi (raw-attr "doi"))]
                 ["manual" ;; Technical documentation
                  (make-bib
                        #:type 'manual
                        ;; required:
                        #:title (content-attr "title")
                        ;; optional:
                        #:author (parse-author (raw-attr "author"))
                        #:date (raw-attr "year") ;; TODO: optional month
                        #:location (manual-location
                                      ;; optional:
                                      #:organization (content-attr "organization")
                                      #:edition (content-attr "edition"))
                        #:note (content-attr "note")
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:url (raw-attr "url")
                        #:doi (raw-attr "doi"))]
                 ["mastersthesis" ;; A Master's thesis.
                  (make-bib
                        #:type 'mastersthesis
                        ;; required:
                        #:author (parse-author (raw-attr "author"))
                        #:title (content-attr "title")
                        #:date (raw-attr "year") ;; TODO: optional month
                        #:location (dissertation-location
                                      #:institution (content-attr "school")
                                      #:degree "Master’s"
                                      ;; optional:
                                      #:type (content-attr "type")
                                      #:address (content-attr "address"))
                        ;; optional:
                        #:note (content-attr "note")
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:url (raw-attr "url")
                        #:doi (raw-attr "doi"))]
                 ["misc" ;; Use this type when nothing else fits.
                  (make-bib
                        #:type 'misc
                        ;; optional: (no required field)
                        #:author (parse-author (raw-attr "author"))
                        #:title (content-attr "title")
                        #:date (raw-attr "year") ;; TODO: month
                        #:location (misc-location
                                      #:howpublished (content-attr "howpublished"))
                        ;; optional:
                        #:note (content-attr "note")
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:url (raw-attr "url")
                        #:doi (raw-attr "doi"))]
                 ["phdthesis" ;; A PhD thesis.
                  (make-bib
                        #:type 'phdthesis
                        ;; required:
                        #:author (parse-author (raw-attr "author"))
                        #:title (content-attr "title")
                        #:date (raw-attr "year") ;; TODO: optional month
                        #:location (dissertation-location
                                      #:institution (content-attr "school")
                                      #:degree "PhD"
                                      ;; optional:
                                      #:type (content-attr "type")
                                      #:address (content-attr "address"))
                        ;; optional:
                        #:note (content-attr "note")
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:url (raw-attr "url")
                        #:doi (raw-attr "doi"))]
                 ["proceedings" ;; The proceedings of a conference.
                  (make-bib
                        #:type 'proceedings
                        ;; required:
                        #:title (content-attr "title")
                        #:date (raw-attr "year") ;; TODO: optional month
                        ;; optional:
                        #:location (proceedings-location
                                      (content-attr* "booktitle")
                                      ;; optional:
                                      #:editor (parse-author (raw-attr "editor"))
                                      #:volume (content-attr "volume") ;; volume OR number
                                      #:number (content-attr "number")
                                      #:series (content-attr "series")
                                      #:address (content-attr "address")
                                      #:organization (content-attr "organization")
                                      #:publisher (content-attr "publisher"))
                        #:note (content-attr "note")
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:url (raw-attr "url")
                        #:doi (raw-attr "doi"))]
                 ["techreport" ;; A report published by a school or other institution, usually numbered within a series.
                 ;; Required fields: author, title, institution, year. Optional fields: type, number, address, month, note.
                  (make-bib
                        #:type 'techreport
                        ;; required:
                        #:author (parse-author (raw-attr "author"))
                        #:title (content-attr "title")
                        #:date (raw-attr "year") ;; TODO: optional month
                        #:location (techrpt-location
                                      #:institution (content-attr "institution")
                                      ;; optional:
                                      #:type (content-attr "type")
                                      #:number (content-attr "number")
                                      #:address (content-attr "address"))
                        ;; optional:
                        #:note (content-attr "note")
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:url (raw-attr "url")
                        #:doi (raw-attr "doi"))]
                 ["unpublished" ;; A document having an author and title, but not formally published.
                  (make-bib
                        #:type 'unpublished
                        ;; required:
                        #:author (parse-author (raw-attr "author"))
                        #:title (content-attr "title")
                        #:note (content-attr "note")
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
                        #:title (content-attr "title")
                        #:url (raw-attr "url")
                        #:location (webpage-location
                                     #:accessed (content-attr "urldate")) ;; when visited
                        #:author (parse-author (raw-attr "author"))
                        #:note (content-attr "note")
                        #:date (raw-attr "year") ;; TODO: month ;; presumably when written
                        #:doi (raw-attr "doi"))]
                 ["webpage"
                  (make-bib
                        #:type 'webpage
                        ;; extra: (WHERE IS THAT SPECIFIED?)
                        #:title (content-attr "title")
                        #:url (raw-attr "url")
                        #:location (webpage-location
                                     #:accessed (content-attr "lastchecked"))
                        #:author (parse-author (raw-attr "author"))
                        #:note (content-attr "note")
                        #:date (raw-attr "year") ;; TODO: month ;; presumably when written
                        #:doi (raw-attr "doi"))]
                 [_
                  (make-bib #:title (format "~v" the-raw))]))))

(module+ test
  (require rackunit
           racket/file
           racket/path
           scribble/render
           (prefix-in html: scribble/html-render))
  (define grouping-db
    (bibtex-parse
     (open-input-string
      "@misc{x, author={Guy L. {Steele Jr.}}, title={The {ACM} Paper}}")))
  (check-equal?
   (hash-ref (hash-ref (bibdb-raw grouping-db) "x") "author")
   "Guy L. {Steele Jr.}")
  (check-equal?
   (hash-ref (hash-ref (bibdb-raw grouping-db) "x") "title")
   "The {ACM} Paper")
  (check-equal? (content->string (parse-author "Guy L. {Steele Jr.}"))
                "Guy L. Steele Jr.")
  (check-equal? (content->string (parse-author "{Steele Jr.}, Guy L."))
                "Guy L. Steele Jr.")
  (check-equal? (content->string (parse-author "Steele, Jr., Guy L."))
                "Guy L. Steele Jr.")
  (check
   print-as-equal-string?
   (parse-author "Guy L. {Steele Jr.}")
   (authors
    (author-name "Guy L."
                 (latex->content "{Steele Jr.}"))))
  (check
   print-as-equal-string?
   (parse-author "Steele, Jr., Guy L.")
   (authors
    (author-name "Guy L." "Steele" #:suffix "Jr.")))
  (check-equal?
   (content->string
    (parse-author "{Barnes and Noble, Inc.} and Guy L. {Steele Jr.}"))
   "Barnes and Noble, Inc. and Guy L. Steele Jr.")
  (check-true
   (bibtex-group?
    (name-word-content
     (last (filter name-word?
                   (name-tokens (latex->content "Guy L. {Steele Jr.}")))))))
  (check-equal? (content->string (latex->content "\\emph{a \\texttt{b}}"))
                "a b")
  (define required-db
    (bibtex-parse
     (open-input-string
      #<<BIB
@article{journal-test,
  author={Alice Example},
  title={An Article},
  journal={\emph{JournalSentinel}},
  year={2026}
}
@inproceedings{conference-test,
  author={Bob Example},
  title={A Paper},
  booktitle={\texttt{ConferenceSentinel}},
  year={2026}
}
@proceedings{proceedings-test,
  title={Collected Papers},
  booktitle={\emph{ProceedingsSentinel}},
  year={2026}
}
BIB
      )))

  (define-cite test-cite test-citet test-bibliography)

  (void
   (test-cite
    (generate-bib required-db "journal-test")
    (generate-bib required-db "conference-test")
    (generate-bib required-db "proceedings-test")))

  (define html-path
    (make-temporary-file "bibtex-required~a.html"))

  (render (list (test-bibliography))
          (list html-path)
          #:dest-dir (path-only html-path)
          #:render-mixin html:render-mixin)

  (define rendered (file->string html-path))

  ;; Required fields must retain LaTeX formatting.
  (check-true
   (regexp-match? #px"<em[^>]*>JournalSentinel</em>"
                  rendered))
  (check-true
   (regexp-match? #px"<span[^>]*class=\"stt\"[^>]*>ConferenceSentinel</span>"
                  rendered))
  (check-true
   (regexp-match? #px"<em[^>]*>ProceedingsSentinel</em>"
                  rendered))

  (delete-file html-path)

  ;; Required fields must still produce useful errors when absent.
  (check-exn
   #rx"missing attribute journal"
   (λ ()
     (generate-bib
      (bibtex-parse
       (open-input-string
        "@article{x, title={X}, year={2026}}"))
      "x")))

  (check-exn
   #rx"missing attribute booktitle"
   (λ ()
     (generate-bib
      (bibtex-parse
       (open-input-string
        "@inproceedings{x, title={X}, year={2026}}"))
      "x"))))

(provide (struct-out bibdb)
         path->bibdb
         bibtex-parse
         define-bibtex-cite
         define-bibtex-cite*)

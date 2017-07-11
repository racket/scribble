#lang racket/base

(require rackunit scriblib/autobib scribble/base scribble/core)

(test-case "define-cite"
  ;; Check that `define-cite` binds the expected identifiers

  (let ()
    (define-cite cite citet gen-bib)
    (check-pred values (void cite citet gen-bib)))

  (let ()
    (define-cite cite citet gen-bib
      #:cite-author cite-author
      #:cite-year cite-year)
    (check-pred values (void cite citet gen-bib cite-author cite-year))))

(test-case "proceedings-location"
  (check-not-exn
    (λ () (proceedings-location "RacketCon" #:pages '(1 2) #:series 3 #:volume 4)))
  (check-not-exn
    (λ () (proceedings-location 'PLDI)))
  (check-exn exn:fail:contract?
    (λ () (proceedings-location "USENIX" #:pages "4--5"))))

(test-case "journal-location"
  (check-not-exn
    (λ () (journal-location "CACM" #:pages '(1 2) #:number 3 #:volume 4)))
  (check-not-exn
    (λ () (journal-location 'JFP)))
  (check-exn exn:fail:contract?
    (λ () (journal-location "Journal of Chromatography" #:pages 30))))

(test-case "book-location"
  (check-not-exn
    (λ () (book-location #:edition 1 #:publisher "A.C. Clayton")))
  (check-not-exn
    (λ () (book-location #:edition 'B #:publisher 'Elsiver)))
  (check-exn exn:fail?
    (λ () (book-location))))

(define (mk-bookloc-elem/ed ed) (element (style #f '()) (list ed " edition")))

(test-case "book-location-edition-capitalization"
  (check-equal? (book-location #:edition 'a)
                (mk-bookloc-elem/ed "A"))
  (check-equal? (book-location #:edition "first")
                (mk-bookloc-elem/ed "First"))
  (check-equal? (book-location #:edition 'Third)
                (mk-bookloc-elem/ed "Third"))
  (check-equal? (book-location #:edition 1)
                (mk-bookloc-elem/ed "1"))
  (check-equal? (book-location #:edition "1st")
                (mk-bookloc-elem/ed "1st"))
  (check-equal? (book-location #:edition "4th")
                (mk-bookloc-elem/ed "4th")))


(test-case "techrpt-location"
  (check-not-exn
    (λ () (techrpt-location #:institution "MIT" #:number 'AIM-353)))
  (check-exn exn:fail:contract?
    (λ () (techrpt-location #:institution 'UCB))))

(test-case "dissertation-location"
  (check-not-exn
    (λ () (dissertation-location #:institution "New College")))
  (check-not-exn
    (λ () (dissertation-location #:institution 'Oberlin)))
  (check-not-exn
    (λ () (dissertation-location #:institution "Georgetown University" #:degree "BS")))
  (check-exn exn:fail:contract?
    (λ () (dissertation-location #:degree "PhD"))))

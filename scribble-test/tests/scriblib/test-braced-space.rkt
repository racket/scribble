#lang racket
(require scriblib/bibtex
         racket/runtime-path
         rackunit)

(define-runtime-path test.bib "test-braced-space.bib")

(define db (path->bibdb test.bib))
(define raw (bibdb-raw db))

;; Test that spaces after braced strings are preserved
(test-case "braced strings should preserve following spaces"
  (define test1 (hash-ref raw "test-braced1"))
  (define test2 (hash-ref raw "test-braced2"))
  (define test3 (hash-ref raw "test-braced3"))

  (check-equal? (hash-ref test1 "title") "Advances in ACM Technology")
  (check-equal? (hash-ref test1 "journal") "Proceedings of the IEEE Conference")
  (check-equal? (hash-ref test2 "title") "The ACM Framework")
  (check-equal? (hash-ref test3 "title") "Testing SIGPLAN Methods")
  (check-equal? (hash-ref test3 "booktitle") "International ACM Conference"))

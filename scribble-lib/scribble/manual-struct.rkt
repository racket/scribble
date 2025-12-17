#lang racket/base
(require "core.rkt"
         "private/provide-structs.rkt"
         racket/contract/base)

(define kind/c (listof (or/c string? (list/c 'code string?))))
(define desc-extras/c (make-extras/c))

(define-syntax-rule (make-extras/c quasi-field ...)
  (hash/dc [k symbol?]
           [v (k)
              (case k
                quasi-field
                ...
                ;; keys expected at both `index-desc` and `exported-index-desc*` layers:
                [(language-family) (listof string?)]
                [(sort-order) real?]
                [(kind) string?]
                [(module-kind) (or/c 'lib 'lang 'reader)]
                [(part?) boolean?]
                [(hidden?) boolean?]
                [else any/c])]
           #:immutable #t
           #:kind 'flat))

(provide desc-extras/c)

(provide-structs
 [index-desc ([extras desc-extras/c])]
 [module-path-index-desc ()]
 [(language-index-desc module-path-index-desc) ()]
 [(reader-index-desc module-path-index-desc) ()]
 [exported-index-desc ([name symbol?]
                       [from-libs (listof module-path?)])]
 [(exported-index-desc* exported-index-desc) ([extras (make-extras/c
                                                       [(method-name) symbol?]
                                                       [(display-from-libs) (listof content?)]
                                                       [(constructor?) boolean?]
                                                       [(class-tag) tag?]
                                                       [(long-key) string?])])]
 [(method-index-desc exported-index-desc) ([method-name symbol?]
                                           [class-tag tag?])]
 [(constructor-index-desc exported-index-desc) ([class-tag tag?])]
 [(procedure-index-desc exported-index-desc) ()]
 [(thing-index-desc exported-index-desc) ()]
 [(struct-index-desc exported-index-desc) ()]
 [(form-index-desc exported-index-desc) ()]
 [(class-index-desc exported-index-desc) ()]
 [(interface-index-desc exported-index-desc) ()]
 [(mixin-index-desc exported-index-desc) ()])

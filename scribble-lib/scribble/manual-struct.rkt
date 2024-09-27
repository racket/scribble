#lang racket/base
(require "core.rkt"
         "private/provide-structs.rkt"
         racket/contract/base)

(define kind/c (listof (or/c string? (list/c 'code string?))))

(provide-structs
 [module-path-index-desc ()]
 [(language-index-desc module-path-index-desc) ()]
 [(reader-index-desc module-path-index-desc) ()]
 [exported-index-desc ([name symbol?]
                       [from-libs (listof module-path?)])]
 [(exported-index-desc* exported-index-desc) ([extras (hash/dc [k symbol?]
                                                               [v (k)
                                                                  (case k
                                                                    [(kind) string?]
                                                                    [(hidden?) boolean?]
                                                                    [(method-name) symbol?]
                                                                    [(constructor?) boolean?]
                                                                    [(class-tag) tag?]
                                                                    [else any/c])]
                                                               #:immutable #t)])]
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

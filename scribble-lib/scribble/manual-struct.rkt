#lang racket/base
(require "core.rkt"
         "private/provide-structs.rkt"
         racket/contract/base)

(provide-structs
 [module-path-index-desc ()]
 [(language-index-desc module-path-index-desc) ()]
 [(reader-index-desc module-path-index-desc) ()]
 [exported-index-desc ([name symbol?]
                       [from-libs (listof module-path?)])]
 [(method-index-desc exported-index-desc) ([method-name symbol?]
                                           [class-tag tag?])]
 [(constructor-index-desc exported-index-desc) ([class-tag tag?])]
 [(procedure-index-desc exported-index-desc) ()]
 [(procedure-index-desc* procedure-index-desc) ([kind string?])]
 [(thing-index-desc exported-index-desc) ()]
 [(thing-index-desc* thing-index-desc) ([kind string?])]
 [(struct-index-desc exported-index-desc) ()]
 [(form-index-desc exported-index-desc) ()]
 [(form-index-desc* form-index-desc) ([kind string?])]
 [(class-index-desc exported-index-desc) ()]
 [(interface-index-desc exported-index-desc) ()]
 [(mixin-index-desc exported-index-desc) ()])

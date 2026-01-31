#lang racket/base
(require racket/contract/base
         racket/serialize
         "core.rkt"
         (submod "racket.rkt" id-element)
         "private/manual-bind.rkt"
         "manual-struct.rkt")

(provide
 (contract-out
  [make-id-element
   (->* (identifier?
         string?
         any/c)
        (#:space (or/c #f symbol?)
         #:suffix (or/c #f symbol? serializable?)
         #:link-style (or/c #f style?)
         #:unlinked-ok? any/c)
        content?)]
  [id-to-target-maker
   (->* (identifier?
         any/c)
        (#:space (or/c #f symbol?)
         #:suffix (or/c #f symbol? serializable?))
        (-> content?
            (-> tag? content?)
            content?))]
  [id-to-form-target-maker
   (->* (identifier?
         any/c)
        (#:space (or/c #f symbol?)
         #:suffix (or/c #f symbol? serializable?))
        (-> content?
            (-> tag? content?)
            content?))]
  [annote-exporting-library
   (->* (content?)
        (#:format-module-path (any/c . -> . string?))
        content?)]
  [with-exporting-libraries
    (->* ((list? . -> . exported-index-desc?))
         delayed-index-desc?)]))

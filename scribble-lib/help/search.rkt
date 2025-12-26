#lang racket/base
(require racket/lazy-require)

;; this indirection is because these bindings
;; are now provided by the "racket-index" package,
;; which depends on this package (and not vice versa)
(lazy-require
 [help (perform-search
        send-main-page
        send-language-family-page)])

(provide perform-search
         send-main-page
         send-language-family-page)

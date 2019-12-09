#lang scheme/base
(require "private/provide-structs.rkt"
         racket/contract/base
         xml/xexpr
         net/url-string
         net/url-structs)

(provide-structs
 [body-id ([value string?])]
 [document-source ([module-path module-path?])]
 [package-source ([base url?] [subpath-target (or/c symbol? boolean?)])]

 [xexpr-property ([before xexpr/c] [after xexpr/c])]
 [hover-property ([text string?])]
 [script-property ([type string?]
                   [script (or/c path-string? (listof string?))])]
 [css-addition ([path (or/c path-string? (cons/c 'collects (listof bytes?)) url? bytes?)])]
 [js-addition ([path (or/c path-string? (cons/c 'collects (listof bytes?)) url? bytes?)])]
 [html-defaults ([prefix-path (or/c bytes? path-string? (cons/c 'collects (listof bytes?)))]
                 [style-path (or/c bytes? path-string? (cons/c 'collects (listof bytes?)))]
                 [extra-files (listof (or/c path-string? (cons/c 'collects (listof bytes?))))])]
 [css-style-addition ([path (or/c path-string? (cons/c 'collects (listof bytes?)) url? bytes?)])]
 [js-style-addition ([path (or/c path-string? (cons/c 'collects (listof bytes?)) url? bytes?)])]

 [url-anchor ([name string?])]
 [alt-tag ([name (and/c string? #rx"^[a-zA-Z0-9]+$")])]
 [attributes ([assoc (listof (cons/c symbol? string?))])]
 [column-attributes ([assoc (listof (cons/c symbol? string?))])]

 [part-link-redirect ([url url?])]
 [install-resource ([path path-string?])]
 [link-resource ([path path-string?])]

 [head-extra ([xexpr xexpr/c])]
 [render-convertible-as ([types (listof (or/c 'png-bytes 'svg-bytes))])])


;; Extra constructors for package-source struct

(define (exact-source-url x)
  (define u (if (url? x) x (string->url x)))
  (package-source u #false))

(define (github-source-url user repo #:path [path #f] #:branch [pre-branch #f])
  (define branch (if pre-branch (format "#~a" pre-branch) ""))
  (define u (string->url (format "https://github.com/~a/~a?path=~a~a" user repo (or path "") branch)))
  (package-source u 'path))

(define (gitlab-source-url user repo #:path [pre-path #f] #:branch [pre-branch #f])
  (define branch (or pre-branch "master"))
  (define path (if pre-path (format "/~a" pre-path) ""))
  (define u (string->url (format "https://gitlab.com/~a/~a/blob/~a~a" user repo branch path)))
  (package-source u #true))

(define git-source-url/c
  (->* [(or/c symbol? string?) (or/c symbol? string?)]
       [#:path (or/c #f symbol? string?)
        #:branch (or/c #f symbol? string?)]
       package-source?))

(provide
  (contract-out
    [exact-source-url (-> (or/c url? string?) package-source?)]
    [github-source-url git-source-url/c]
    [gitlab-source-url git-source-url/c]))

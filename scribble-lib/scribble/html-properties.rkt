#lang racket/base
(require "private/provide-structs.rkt"
         racket/contract/base
         xml/xexpr
         net/url-structs)

(provide-structs
 [body-id ([value string?])]
 [document-source ([module-path module-path?])]

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
 [part-title-and-content-wrapper ([tag string?]
                                  [attribs (listof (list/c symbol? string?))])]
 [install-resource ([path path-string?])]
 [link-resource ([path path-string?])]

 [head-extra ([xexpr xexpr/c])]
 [head-addition ([xexpr xexpr/c])]
 [render-convertible-as ([types (listof (or/c 'png-bytes 'svg-bytes 'gif-bytes))])])

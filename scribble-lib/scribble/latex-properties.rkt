#lang scheme/base
(require "private/provide-structs.rkt"
         racket/serialize
         racket/contract/base)

(provide-structs
 [tex-addition ([path (or/c path-string? (cons/c 'collects (listof bytes?)) bytes?)])]
 [latex-defaults ([prefix (or/c bytes? path-string? (cons/c 'collects (listof bytes?)))]
                  [style (or/c bytes? path-string? (cons/c 'collects (listof bytes?)))]
                  [extra-files (listof (or/c path-string? (cons/c 'collects (listof bytes?))))])]
 [(latex-defaults+replacements latex-defaults)
  ([replacements (hash/c string? (or/c bytes? path-string? (cons/c 'collects (listof bytes?))))])]
 [command-extras ([arguments (listof string?)])]
 [command-optional ([arguments (listof string?)])]
 [short-title ([text (or/c string? #f)])]
 [table-row-skip ([amount string?])])

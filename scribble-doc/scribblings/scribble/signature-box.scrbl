#lang scribble/doc
@(require scribble/manual "utils.rkt"
          (for-label scribble/core
                     scribble/signature-box
                     racket/contract
                     setup/xref))

@title[#:tag "signature-box"]{Signature Boxes Utilities}

@defmodule[scribble/signature-box]{
  The @racketmodname[scribble/signature-box] provides access
  to the content of the ``signature boxes'' that describe
  some module's export (but without any styling).}

@defproc[(fetch-signature-box-strs [tag tag?]
                               [#:signature-box-cache signature-box-cache
                                                  signature-box-cache?
                                                  (make-signature-box-cache #t)])
         (or/c #f (non-empty-listof string?))]{
  Returns a list of strings that show the content of the signature box
  (without any styling information) for the documentation referenced
  by @racket[tag].

  The first string in the list describes the export (e.g. @racket["procedure"]
  when @racket[defproc] is used, or @racket["syntax"] when @racket[defform]
  was used to document the export).
}

@defproc[(fetch-signature-box-method-tags [method-name symbol?]
                                      [#:signature-box-cache signature-box-cache
                                                         signature-box-cache?
                                                         (make-signature-box-cache #t)])
         (listof method-tag?)]{
  Returns the list of tags for all methods that are documented in the documentation
  in @racket[signature-box-cache].

  @history[#:added "1.11"]
}

@defproc[(make-signature-box-cache
          [populate? boolean?]
          [#:signature-box-dirs signature-box-dirs (listof path?) (get-doc-search-dirs)])
         signature-box-cache?]{
  Constructs a new (mutable) signature-box cache.
  
  If @racket[populate?] is @racket[#f], the cache is initially
  unpopulated, in which case it is filled in the first time the cache
  is passed to @racket[fetch-signature-box-strs]. Otherwise, the cache is
  populated immediately.

  The @racket[signature-box-dirs] argument is a list of directories that are
  looked inside for @filepath{signature-box.rktd} files. The default value
  is only an approximation for where those files usually reside. See
  also @racket[get-rendered-doc-directories].
}

@defproc[(signature-box-cache? [v any/c]) boolean?]{
  Determines if @racket[v] is a signature-box cache.
}

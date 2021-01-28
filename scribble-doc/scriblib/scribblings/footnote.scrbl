#lang scribble/manual
@(require (for-label scribble/struct
                     scriblib/footnote
                     racket/base
                     scheme/contract))

@title[#:tag "footnotes"]{Footnotes}

@defmodule[scriblib/footnote]

@defproc[(note [pre-content pre-content?] ...) element?]{

Creates a margin note for HTML and a footnote for Latex/PDF output.}

@defform[(define-footnote footnote-id footnote-part-id)]{

Binds @racket[footnote-id] to a form like @racket[note] that registers a
footnote.
Binds @racket[footnote-part-id] to a function that generates a section to
display the registered footnotes.
(The section generated by @racket[footnote-part-id] will not show a title or
appear in a table of contents; it will look like a footnote area.)

Beware that any content passed to @racket[footnote-id] will occur
twice in at least an intermediate form of the document, and perhaps
also in the rendered form of the document. Consequently, the content
passed to @racket[footnote-id] should not bind link targets or include
other one-time declarations.}

Example:
@codeblock|{
  #lang scribble/manual
  @require[scriblib/footnote]

  @define-footnote[my-note make-my-note]

  @title{Months of the Year}

  @section{January}
  January has 31 days.

  @section{February}
  February has 28 days in common years.@my-note{In leap years,
  February has 29 days.}

  @make-my-note[]

  @section{March}
  March has 30 days.
}|

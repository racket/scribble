#lang scribble/manual
@(require (for-label scribble/core
                     racket/base
                     scriblib/book-index))

@title[#:tag "book-index"]{Book-Style Indexing}

@defmodule[scriblib/book-index]{Provides a list of style properties to
attach to a Scribble document that contains an index part, making the
index more suitable for a traditional rendering on paper. The style
properties cause index entries to be merged when they have the same
content, with (potentially) multiple page numbers attached to the
merged entry.}

@defthing[book-index-style-properties list?]{

Combine these style properties with others for the style of a part
(typically specified in @racket[title]) for a document that contains
an index. The style properties enable index merging and select an
implementation based on the @tt{cleveref} Latex package.

Example:

@codeblock[#:keep-lang-line? #t]|{
#lang scribble/base
@(require scriblib/book-index
          (only-in scribble/core make-style))

@title[#:style (make-style #f book-index-style-properties)]{Demo}

This paragraph is about @as-index{examples}.

This paragraph is about @as-index{examples}, too.

@index-section[]}|}

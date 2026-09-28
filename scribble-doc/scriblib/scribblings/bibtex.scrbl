#lang scribble/manual
@(require (for-label scribble/struct
                     scriblib/bibtex
                     scriblib/autobib
                     racket/base
                     racket/contract))

@title[#:tag "bibtex"]{BibTeX Bibliographies}

@defmodule[scriblib/bibtex]

This library supports parsing BibTeX @litchar{.bib} files.

We support the 14 standard BibTeX entry types documented in Oren
Patashnik's
@hyperlink["https://ctan.org/pkg/bibtex"]{@italic{BIBTEXing}}
(February 8, 1988), §3.1 "Entry Types" — the definitive reference for
classic BibTeX, which updates Appendix B.2 of Leslie Lamport's
@italic{LaTeX: A Document Preparation System} (1986):
@litchar{article}, @litchar{book}, @litchar{booklet}, @litchar{conference},
@litchar{inbook}, @litchar{incollection}, @litchar{inproceedings},
@litchar{manual}, @litchar{mastersthesis}, @litchar{misc}, @litchar{phdthesis},
@litchar{proceedings}, @litchar{techreport}, and @litchar{unpublished}.

Human-readable BibTeX fields are converted from a subset of
LaTeX syntax into Scribble content. This includes grouping
braces, common accent and special-letter commands,
@tt{\emph}, @tt{\texttt}, @tt{\textit}, @tt{\textbf},
@tt{\textsc} and @tt{\url}.

Grouping braces are preserved internally where relevant
to BibTeX name parsing. Unknown LaTeX commands and their
attached arguments are retained rather than discarded.
Inline and display mathematics are preserved in LaTeX
syntax; other backends do not translate them into native
mathematical expressions.

Blank lines in @litchar{note} fields separate paragraphs.

The @litchar{url} and @litchar{doi} fields are interpreted
as scalar strings rather than general LaTeX content.
Enclosing brace groups are removed and conventional
LaTeX escapes for special URL characters, such as
@litchar{\_}, @litchar{\%} and @litchar{\&}, are unescaped.
Other LaTeX commands are not interpreted in these fields.

We support all the required and optional fields documented in
@italic{BIBTEXing}, with the following known limitations so far:
@itemize[
  @item{We fail to process @litchar{month}.}
  @item{We only support @litchar{pages} fields that have decimal numbers
        separated by one or more dashes.}
  @item{We fail to process the @litchar{type} optional field of @litchar{incollection}.}
  @item{We do not at this time support the non-standard but often seen fields
        @litchar{isbn}, @litchar{issn}, nor any other non-standard field
        except those described below.}]

For each standard entry type, the fields that
@hyperlink["https://ctan.org/pkg/bibtex"]{@italic{BIBTEXing}} §3.1 marks
required are enforced at parse time: a missing one raises an error,
rather than being silently treated as absent the way every other field
is. This is:
@itemize[
  @item{@litchar{article}: @litchar{author}, @litchar{title},
        @litchar{journal}, @litchar{year}.}
  @item{@litchar{book}: @litchar{author} or @litchar{editor},
        @litchar{title}, @litchar{publisher}, @litchar{year}.}
  @item{@litchar{booklet}: @litchar{title}.}
  @item{@litchar{conference}: same as @litchar{inproceedings}.}
  @item{@litchar{inbook}: @litchar{author} or @litchar{editor},
        @litchar{title}, @litchar{chapter} and/or @litchar{pages},
        @litchar{publisher}, @litchar{year}.}
  @item{@litchar{incollection}: @litchar{author}, @litchar{title},
        @litchar{booktitle}, @litchar{publisher}, @litchar{year}.}
  @item{@litchar{inproceedings}: @litchar{author}, @litchar{title},
        @litchar{booktitle}, @litchar{year}.}
  @item{@litchar{manual}: @litchar{title}.}
  @item{@litchar{mastersthesis}: @litchar{author}, @litchar{title},
        @litchar{school}, @litchar{year}.}
  @item{@litchar{misc}: none.}
  @item{@litchar{phdthesis}: @litchar{author}, @litchar{title},
        @litchar{school}, @litchar{year}.}
  @item{@litchar{proceedings}: @litchar{title}, @litchar{year}.}
  @item{@litchar{techreport}: @litchar{author}, @litchar{title},
        @litchar{institution}, @litchar{year}.}
  @item{@litchar{unpublished}: @litchar{author}, @litchar{title},
        @litchar{note} (no @litchar{year}).}]
An @litchar{author}-or-@litchar{editor} or
@litchar{chapter}-and/or-@litchar{pages} requirement above is enforced
as: an error unless at least one of the two fields is present.

Note that this is stricter than classic BibTeX itself: per
@italic{BIBTEXing}, "required" is a property of the standard
bibliography styles, not a constraint on the @tt{.bib} file format
itself — a real BibTeX run only warns about a missing required field
and still processes the entry, however poorly formatted the result.
We chose to make it a hard error here instead, on the theory that a
citation silently missing its journal or its year is worse than one
that fails to build at all.

In addition to the 14 standard entries, we support the often seen
@litchar{online} and @litchar{webpage} entry types,
for which we support the fields @litchar{url}, @litchar{title}, @litchar{author}.
Additionally @litchar{online} has field @litchar{urldate} for the day the site was visited,
whereas @litchar{webpage} instead has field @litchar{lastchecked}.
Neither is among @italic{BIBTEXing}'s standard entry types, so there is
no spec to draw a required-fields list from; but by our own choice,
@litchar{title} and @litchar{url} are required for both (a webpage
citation with neither isn't a citation), while @litchar{author} stays
optional, since most web pages don't have a clean byline.

Also, for every entry type, we support the extra fields
@litchar{note}, @litchar{url}, @litchar{doi} — all optional, except that
@litchar{url} is required (see above) for @litchar{online} and
@litchar{webpage}, the two entry types it's actually about.
But mind that the @litchar{doi} field currently overrides the @litchar{url}
in @racketmodname[scriblib/autobib].

We do support the @litchar["@string"] feature defined in
@hyperlink["https://www.bibtex.org/Format/"]{the format of BibTeX}.

@history[#:changed "1.61"
  @elem{Support all standard entry types plus @litchar{online} and @litchar{webpage},
  all fields but @litchar{month} (or @litchar{type} for @litchar{incollection}),
  and support @litchar{note}, @litchar{url}, @litchar{doi} on all entry types.}]
@history[#:changed "1.68"
  @elem{Added structured LaTeX content parsing, improved
        author-name handling, URL and DOI unescaping,
        and support for multi-paragraph notes. Started enforcing,
        as hard parse-time errors, the fields
        @italic{BIBTEXing} marks required for each standard entry
        type (see above), including its author-or-editor and
        chapter-and/or-pages disjunctions; also require
        @litchar{title} and @litchar{url} on @litchar{online} and
        @litchar{webpage} entries, by our own choice rather than any
        spec. Fixed a latent bug where an @litchar{editor}-only
        @litchar{book} or @litchar{inbook} entry (valid, since author
        is not required when editor is given) would silently render
        a bogus @tt{"#f"} in place of the missing author's name.}]

@defform[(define-bibtex-cite bib-pth ~cite-id citet-id generate-bibliography-id
           option ...)]{

Expands into:
@racketblock[
(begin
  (define-cite autobib-cite autobib-citet generate-bibliography-id
     option ...)
  (define-bibtex-cite* bib-pth
    autobib-cite autobib-citet
    ~cite-id citet-id))]
}

@defform[(define-bibtex-cite* bib-pth autobib-cite autobib-citet
                              ~cite-id citet-id)]{

Parses @racket[bib-pth] as a BibTeX database, and augments
@racket[autobib-cite] and @racket[autobib-citet] into
@racket[~cite-id] and @racket[citet-id] functions so that rather than
accepting @racket[bib?] structures, they accept citation key strings.

Each string is broken along spaces into citations keys that are looked up in the BibTeX database and turned into @racket[bib?] structures.
}

@defstruct*[bibdb ([raw (hash/c string? (hash/c string? string?))]
                   [bibs (hash/c string? bib?)])]{
                                             Represents a BibTeX database. The @racket[_raw] hash table maps the labels in the file to hash tables of the attributes and their values. The @racket[_bibs] hash table maps the same labels to Scribble data-structures representing the same information.
                                             }

@defproc[(path->bibdb [path path-string?])
         bibdb?]{
                 Parses a path into a BibTeX database.
                 }

@defproc[(bibtex-parse [ip input-port?])
         bibdb?]{
                 Parses an input port into a BibTeX database.
                 }

#lang scribble/manual
@(require (for-label scribble/struct
                     scriblib/bibtex
                     scriblib/autobib
                     racket/base
                     racket/contract))

@title[#:tag "bibtex"]{BibTeX Bibliographies}

@defmodule[scriblib/bibtex]

This library supports parsing BibTeX @litchar{.bib} files.

We support the 14 BibTeX entry types documented
@hyperlink["https://www.openoffice.org/bibliographic/bibtex-defs.html"]{in the LaTeX book}
(1986) Appendix B.2:
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

We support all the required and optional fields documented in the LaTeX book,
with the following known limitations so far:
@itemize[
  @item{We fail to process @litchar{month}.}
  @item{We only support @litchar{pages} fields that have decimal numbers
        separated by one or more dashes.}
  @item{We fail to process the @litchar{type} optional field of @litchar{incollection}.}
  @item{We do not at this time support the non-standard but often seen fields
        @litchar{isbn}, @litchar{issn}, nor any other non-standard field
        except those described below.}]

In addition to the old standard entries, we support the often seen
@litchar{online} and @litchar{webpage} entry types,
for which we support the fields @litchar{url}, @litchar{title}, @litchar{author}.
Additionally @litchar{online} has field @litchar{urldate} for the day the site was visited,
whereas @litchar{webpage} instead has field @litchar{lastchecked}.

Also, for every entry type, we support the extra optional fields
@litchar{note}, @litchar{url}, @litchar{doi}.
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
        and support for multi-paragraph notes.}]

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

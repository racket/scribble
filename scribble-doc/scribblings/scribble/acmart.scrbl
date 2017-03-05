#lang scribble/manual
@(require "utils.rkt" (for-label (except-in scribble/acmart title author)))


@title{ACM Paper Format}

@defmodulelang[scribble/acmart]{The @racketmodname[scribble/acmart]
language is like @racketmodname[scribble/base], but configured with
LaTeX style defaults to use the @filepath{acmart.cls} class
file that is included with Scribble.}

@(define-syntax-rule (defacmartid name)
   @defidform[name]{
Enables the @tt[(symbol->string 'name)] option. Use @racket[name] only on the
same line as @hash-lang[], with only whitespace (or other options) between
@racketmodname[scribble/acmart] and @racket[name]:
@verbatim[#:indent 2]{
  #lang scribble/acmart @"@"@(symbol->string 'name)
}})

@defacmartid[manuscript]
@defacmartid[acmsmall]
@defacmartid[acmlarge]
@defacmartid[acmtog]
@defacmartid[sigconf]
@defacmartid[siggraph]
@defacmartid[sigplan]
@defacmartid[sigchi]
@defacmartid[sigchi-a]
@defacmartid[review]
@defacmartid[screen]
@defacmartid[natbib]
@defacmartid[anonymous]
@defacmartid[authorversion]

The @racket[manuscript], @racket[acmsmall], @racket[acmlarge], 
@racket[acmtog], @racket[sigconf], @racket[siggraph], @racket[sigplan],
@racket[sigchi], and @racket[sigchi-a] options are all mutually exclusive,
but may each be used in combination of @racket[review], @racket[screen], @racket[natbib],
@racket[anonymous], and @racket[authorversion].


@defproc[(abstract [pre-content pre-content?] ...) block?]{

Generates a @tech{nested flow} for a paper abstract.}

@defform[(include-abstract module-path)]{

Similar to @racket[include-section], but incorporates the document in the
specified module as an abstract. The document must have no title or
sub-parts.}

@defproc[(subtitle [pre-content pre-content?] ...) content?]{

Use as the last argument to @racket[title] to specify a subtitle.}

@defproc[(maketitle) block?]{

Issues the @tt{\maketitle} command.  This must appear after the abstract
any several other top-matter commands.  (See the acmart latex documentation.)}


@deftogether[(
@defproc[(acmJournal [journal pre-content?] ...) content?]
@defproc[(acmConference [name pre-content?] [date pre-content?] [venue pre-content?]) content?]
@defproc[(acmVolume [content pre-content?] ...) content?]
@defproc[(acmNumber [content pre-content?] ...) content?]
@defproc[(acmArticle [content pre-content?] ...) content?]
@defproc[(acmYear [content pre-content?] ...) content?]
@defproc[(acmMonth [content pre-content?] ...) content?]
@defproc[(acmArticleSeq [content pre-content?] ...) content?]
@defproc[(acmPrice [content pre-content?] ...) content?]
@defproc[(acmISBN [content pre-content?] ...) content?]
@defproc[(acmDOI [content pre-content?] ...) content?]
@defproc[(acmBadgeL [content pre-content?] ...) content?]
@defproc[(acmBadgeR [content pre-content?] ...) content?]
)]{

Declares information that is collected into the front-matter region of the paper.}

@defproc[(affiliation [content pre-content?] ...) content?]{

Declares information about the affiliation of an author.}

@deftogether[(
@defproc[(position [content pre-content?] ...) content?]
@defproc[(institution [content pre-content?] ...) content?]
@defproc[(department [content pre-content?] ...) content?]
@defproc[(streetaddress [content pre-content?] ...) content?]
@defproc[(city [content pre-content?] ...) content?]
@defproc[(state [content pre-content?] ...) content?]
@defproc[(postcode [content pre-content?] ...) content?]
@defproc[(country [content pre-content?] ...) content?]
)]{

Declares information that is collected for each author.  These commands should
 only be used within an @racket[affiliation] command.}

@deftogether[(
@defproc[(terms [content pre-content?] ...) content?]
@defproc[(keywords [content pre-content?] ...) content?]
)]{

Typesets term and keyword information for the paper, which
is normally placed immediately after an @racket[abstract] form.
See also @url["http://www.acm.org/about/class/how-to-use"].

For @racket[terms], each general term should be in titlecase. Terms
are usually drawn from a fixed list, and they are usually optional.

For @racket[keywords], capitalize only the first letter of the first
word, separate phrases by commas, and do not include ``and'' before
the last one. Keywords should be noun phrases, not adjectives.}

@defproc[(startpage [content pre-content?] ...) content?]{
Sets the start page for the paper.}

@defproc[(ccsdesc [content pre-content?] ...) content?]{

Declares CCS description.}

@defproc[(received [content pre-content?] ...) content?]{

Sets the date the paper was received.}

@defproc[(citestyle [content pre-content?] ...) content?]{

Sets the citation style for the paper.}

@defproc[(setcitestyle [content pre-content?] ...) content?]{

Sets customization options for the citation style for the paper.}

@defproc[(teaserfigure [content pre-content?] ...) block?]{

Creates a teaser figure to appear before main text.}

@deftogether[(
@defproc[(sidebar [content pre-content?] ...) block?]
@defproc[(marginfigure [content pre-content?] ...) block?]
@defproc[(margintable [content pre-content?] ...) block?]
)]{

In the @racket[sigchi-a] format, special sidebars,
 tables and figures on the margin.}

@deftogether[(
@defproc[(printonly [content pre-content?] ...) block?]
@defproc[(screenonly [content pre-content?] ...) block?]
@defproc[(anonsuppress [content pre-content?] ...) block?]
)]{
Marks content to be included only for print or screen
editions, or excluded from anonymous editions.}

@defproc[(acks [content pre-content?] ...) block?]{

Creates an acknowledgments section.}

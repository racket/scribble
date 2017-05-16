#lang scribble/manual
@(require (except-in "utils.rkt" title author)
          scribble/example
          (for-label scribble/acmart))

@(define acmart-url
   "http://mirrors.concertpass.com/tex-archive/macros/latex/contrib/acmart/acmart.pdf")

@title{ACM Paper Format}

@defmodulelang[scribble/acmart]{The @racketmodname[scribble/acmart]
language is like @racketmodname[scribble/base], but configured with
LaTeX style defaults to use the @hyperlink[acmart-url]{@tt{acmart}}
class for typesetting publications for the Association of Computing
Machinery.}

@deftogether[(
@defidform[manuscript]
@defidform[acmsmall]
@defidform[acmlarge]
@defidform[acmtog]
@defidform[sigconf]
@defidform[siggraph]
@defidform[sigplan]
@defidform[sigchi]
@defidform[sigchi-a]
)]{

Enables the given document format. Use the format only on the same
line as @hash-lang[], with only whitespace (or other options) between
@racketmodname[scribble/acmart] and the format name:

@verbatim[#:indent 2]|{
  #lang scribble/acmart @acmsmall
}|

The @racket[manuscript], @racket[acmsmall], @racket[acmlarge],
@racket[acmtog], @racket[sigconf], @racket[siggraph],
@racket[sigplan], @racket[sigchi], and @racket[sigchi-a] formats are
all mutually exclusive.}

@deftogether[(
@defidform[review]
@defidform[screen]
@defidform[natbib]
@defidform[anonymous]
@defidform[authorversion]
@defidform[9pt]
@defidform[10pt]
@defidform[11pt]
@defidform[12pt]
)]{

Enables the given document format option. Use the option only on the
same line as @hash-lang[], with only whitespace (or other options)
between @racketmodname[scribble/acmart] and the format option.  Any
number of options may be used:

@verbatim[#:indent 2]|{
  #lang scribble/acmart @acmsmall @review @anonymous @natbib
}|

If multiple font size options are used, all but the last are ignored.
}

@defproc[(abstract [pre-content pre-content?] ...) block?]{

Generates a @tech{nested flow} for a paper abstract.}

@defform[(include-abstract module-path)]{

Similar to @racket[include-section], but incorporates the document in the
specified module as an abstract. The document must have no title or
sub-parts.}

@defproc[(title [#:short short-title pre-content? #f]
                [#:tag tag (or/c string? (listof string?) #f) #f]
                [#:tag-prefix prefix (or/c string? module-path? #f) #f]
                [#:style style (or/c style? string? symbol? #f) #f]
                [#:version version (or/c string? #f) #f]
                [#:date date (or/c string? #f) #f]
                [title pre-content?] ...)
         title-decl?]{

Specifies the title of the document, optionally with a short version of the title for running heads.}

@defproc[(subtitle [pre-content pre-content?] ...) content?]{

Specifies a subtitle.}

@defproc[(author [#:orcid orcid (or/c pre-content? #f) #f]
                 [#:affiliation affiliation
                  (or/c pre-content?
                        affiliation?
                        (listof pre-content?)
                        (listof affiliation?)
                        #f)
                  #f]
                 [#:email email
                  (or/c pre-content? (listof pre-content?) #f)
                  #f]
                 [name pre-content?] ...)
         block?]{

 Specifies an author with an optional email address, affiliation, and/or orcid.
 
}

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
)]{

Declares information that is collected into the front-matter region of the paper.}

@deftogether[(
@defproc[(acmBadgeL [#:url url string? #f] [graphics string?]) content?]
@defproc[(acmBadgeR [#:url url string? #f] [graphics string?]) content?]
)]{

Display a special badge, such as an artifact evaluation badge, on the
left or right of the first page.  If @racket[url] is provided, the
screen version of the image links to the badge authority.

}

@defproc[(email [text pre-content?] ...)
         email?]{
 Creates an @racket[email?] object for use with @racket[author].
}

@defproc[(email? [email any/c]) boolean?]{
                                          
 Returns @racket[#t] if @racket[email] is an @racket[email],
 @racket[#f] otherwise.
}
         

@defproc[(affiliation
          [#:position position (or/c pre-content? #f) #f]
          [#:institution institution (or/c pre-content? institution? #f) #f]
          [#:street-address street-address (or/c pre-content? #f) #f]
          [#:city city (or/c pre-content? #f) #f]
          [#:state state (or/c pre-content? #f) #f]
          [#:postcode postcode (or/c pre-content? #f) #f]
          [#:country country (or/c pre-content? #f) #f])
         affiliation?]{
                       
 Creates an @racket[affiliation?] object for use with @racket[author].
}

@defproc[(affiliation? [aff any/c]) boolean?]{
                                              
 Returns @racket[#t] if @racket[aff] is an
 @racket[affiliation], @racket[#f] otherwise.
}

@defproc[(institution [#:departments departments (or/c pre-content? institution? (listof institution)) '()]
                      [inst institution?] ...)
         institution?]{

 Creates an @racket[institution?] object for use in @racket[author].}

@defproc[(institution? [inst any/c]) boolean]{
                                              
 Returns @racket[#t] if @racket[inst] is an
 @racket[institution], @racket[#f] otherwise.
}

@codeblock|{
  #lang scribble/acmart
  @title{Some Title}
  @author["David Van Horn"
          #:affiliation @affiliation[
                         #:institution
                         @institution[
                          #:departments (list @institution{Department of Computer Science}
                                              @institution{UMIACS})]{
                           University of Maryland}
                         #:city "College Park"
                         #:state "Maryland"]
          #:email "dvanhorn@cs.umd.edu"]}

  @abstract{This is an abstract.}
}|

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

@defproc[(startPage [content pre-content?] ...) content?]{
Sets the start page for the paper.}

@defproc[(ccsdesc [#:number number? #f] [content pre-content?] ...) content?]{

Declares CCS description with optional numeric code.}

@defproc[(received [#:stage stage string? #f] [date string?]) content?]{

Sets the history of the publication.  If @racket[stage] is omitted, it
defaults to @racket{Received} for the first occurrence and
@racket{revised} in subsequent uses.

@codeblock[#:keep-lang-line? #f]|{
  #lang scribble/acmart
  @received{February 2007}
  @received[#:stage "revised"]{March 2009}
  @received[#:stage "accepted"]{June 2009}
}|}

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

Creates an unnumbered section ``Acknowledgments'' section, unless the
@racket[anonymous] mode is selected.}

@deftogether[(
@defproc[(grantsponsor [sponsorID string?] [name string?] [url string?]) content?]
@defproc[(grantnum [#:url url string? #f] [sponsorID string?] [num string?]) content?]
)]{

All financial support @emph{must} be listed using the
@racket[grantsponsor] and @racket[grantnum] commands inside of
@racket[acks].

Here @racket[sponsorID] is the unique ID used to match grants to
sponsors, @racket[name] is the name of the sponsor.  The
@racket[sponsorID] of a @racket[grantnum] must match some
@racket[sponsorID] of a @racket[grantsponsor] command.

@codeblock[#:keep-lang-line? #f]|{
  #lang scribble/acmart
  @acks{
    The author thanks Benjamin Greenman for helpful comments on this
    code. Financial support provided by the @grantsponsor["NSF7000"
    "National Scribble Foundation"]{http://racket-lang.org} under
    grant No.: @grantnum["NSF7000"]{867-5309}.}
}|}

@history[#:added "1.20"]


@section{Updating @filepath{acmart.cls}}

The first time your installation of @racketmodname[scribble/acmart] renders
a document, it downloads a copy of @filepath{acmart.cls} from @url{www.sigplan.org}.
Over time, you may need to update this local copy.

@itemlist[
@item{
  If SIGPLAN issues a new release, delete your @filepath{acmart.cls}.
  The next time @racketmodname[scribble/acmart] runs, it will download a new local copy from @url{www.sigplan.org}.
}
@item{
  If you wish to use the development version of @filepath{acmart.cls}:
  @itemlist[#:style 'ordered
  @item{
    clone the @tt{acmart} repository @url{https://github.com/borisveytsman/acmart},
  }
  @item{
    build @filepath{acmart.cls} from @filepath{acmart.dtx} and @filepath{acmart.ins}, and
  }
  @item{
    replace the local copy with the newly-generated file.
  }
  ]
}
]

The local copy of @filepath{acmart.cls} is stored in the
@racketmodname[scribble/acmart] collection.

@examples[
  (collection-file-path "acmart.cls" "scribble" "acmart")
]

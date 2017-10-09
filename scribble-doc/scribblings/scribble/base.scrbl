#lang scribble/doc
@(require scribble/manual "utils.rkt"
          (for-syntax racket/base)
          (for-label setup/main-collects
          racket/runtime-path))

@(define-syntax def-section-like
   (syntax-rules ()
     [(_ id result/c x ...)
      (defproc (id [#:tag tag (or/c #f string? (listof string?)) #f]
                   [#:tag-prefix tag-prefix (or/c #f string? module-path?) #f]
                   [#:style style (or/c style? #f string? symbol? (listof symbol?)) #f]
                   [pre-content pre-content?] (... ...+))
        result/c
        x ...)]))

@(define-syntax def-elem-proc
   (syntax-rules ()
     [(_ id x ...)
      (defproc (id [pre-content pre-content?] (... ...))
        element?
        x ...)]))
@(define-syntax def-style-proc
   (syntax-rules ()
     [(_ id)
      @def-elem-proc[id]{Like @racket[elem], but with style @racket['id].}]))

@title[#:tag "base"]{Base Document Format}

@defmodulelang[scribble/base]{The @racketmodname[scribble/base]
language provides functions and forms that can be used from code
written either in Racket or with @elem["@"] expressions. It
essentially extends @racketmodname[racket/base], except that top-level
forms within a module using the @racketmodname[scribble/base] language
are treated as document content (like @racketmodname[scribble/doclang]).

The @racketmodname[scribble/base] name can also be used as a library
with @racket[require], in which case it provides only the bindings
defined in this section, and it also does not set the reader or
set the default rendering format to the Racket manual format.}

Functions provided by this library, such as @racket[title] and
@racket[italic], might be called from Racket as

@racketblock[
(title #:tag "how-to" 
       "How to Design " (italic "Great") " Programs")
]

They can also be called with @elem["@"] notation as

@verbatim[#:indent 2]|{
  @title[#:tag "how-to"]{How to Design @italic{Great} Programs}
}|

Although the procedures are mostly designed to be used from @elem["@"]
mode, they are easier to document in Racket mode (partly because we
have @racketmodname[scribble/manual]).

@; ------------------------------------------------------------------------

@section{Document Structure}

@defproc[(title [#:tag tag (or/c #f string? (listof string?)) #f]
                [#:tag-prefix tag-prefix (or/c #f string? module-path?) #f]
                [#:style style (or/c style? #f string? symbol? (listof symbol?)) #f]
                [#:version vers (or/c string? #f) #f]
                [#:date date (or/c string? #f) #f]
                [pre-content pre-content?] ...+)
         title-decl?]{

Generates a @racket[title-decl] to be picked up by @racket[decode] or
@racket[decode-part].  The @tech{decode}d @racket[pre-content] (i.e.,
parsed with @racket[decode-content]) supplies the title content. If
@racket[tag] is @racket[#f], a tag string is generated automatically
from the content. The tag string is combined with the symbol
@racket['part] to form the full tag.

The @racket[style] argument can be a style structure, or it can be one
of the following: a @racket[#f] that corresponds to a ``plain'' style,
a string that is used as a @tech{style name}, a symbol that is used as
a @tech{style property}, or a list of symbols to be used as @tech{style properties}.
For information on styles, see @racket[part]. For example, a style of
@racket['toc] causes sub-sections to be generated as separate pages in
multi-page HTML output.

The @racket[tag-prefix] argument is propagated to the generated
structure (see @secref["tags"]). If @racket[tag-prefix] is a module
path, it is converted to a string using
@racket[module-path-prefix->string].

The @racket[vers] argument is propagated to the @racket[title-decl]
structure. Use @racket[""] as @racket[vers] to suppress version
rendering in the output.

The @racket[date] argument is propagated to the @racket[title-decl]
structure via a @racket[document-date] @tech{style property}. Use
@racket[""] as @racket[date] to suppress date rendering in Latex
output.

The section title is automatically indexed by
@racket[decode-part]. For the index key, leading whitespace and a
leading ``A'', ``An'', or ``The'' (followed by more whitespace) is
removed.}


@def-section-like[section part-start?]{ Like @racket[title], but
 generates a @racket[part-start] of depth @racket[0] to be picked up by
 @racket[decode] or @racket[decode-part].}

@def-section-like[subsection part-start?]{ Like @racket[section], but
 generates a @racket[part-start] of depth @racket[1].}

@def-section-like[subsubsection part-start?]{ Like @racket[section], but
 generates a @racket[part-start] of depth @racket[2].}

@def-section-like[subsubsub*section paragraph?]{ Similar to
 @racket[section], but merely generates a paragraph that looks like an
 unnumbered section heading (for when the nesting gets too deep to
 include in a table of contents).}

@defform[(include-section module-path)]{ Requires @racket[module-path]
 and returns its @racket[doc] export (without making any imports
 visible to the enclosing context). Since this form expands to
 @racket[require], it must be used in a module or top-level context.}

@defproc[(author [auth content?] ...) block?]{

Generates a @racket[paragraph] with style name @racket['author] to
show the author(s) of a document, where each author is represented by
@tech{content}. Normally, this function is used after
@racket[title] for the beginning of a document. See also
@racket[author+email].

Examples:

@codeblock[#:keep-lang-line? #f]|{
  #lang scribble/base
  @author{Alice P. Racketeer}
}|}

@defproc[(author+email [author-name content?]
                       [email string?] 
                       [#:obfuscate? obfuscate? any/c #f])
         element?]{

Combines an author name with an e-mail address. If @racket[obfuscate?]
is true, then the result obscures the e-mail address slightly to avoid
address-harvesting robots.

Note that @racket[author+email] is not a replacement for
@racket[author].  The @racket[author+email] function is often used in
combination with @racket[author].

Examples:

@codeblock[#:keep-lang-line? #f]|{
  #lang scribble/base
  @author[(author+email "Bob T. Scribbler" "bob@racket-lang.org")]
}|}

@; ------------------------------------------------------------------------

@section{Blocks}

@defproc[(para [#:style style (or/c style? string? symbol? #f) #f] 
               [pre-content pre-content?] ...) paragraph?]{

 Creates a @tech{paragraph} containing the @tech{decode}d
 @racket[pre-content] (i.e., parsed with @racket[decode-paragraph]).

 The @racket[style] argument can be a style, @racket[#f] to indicate a
  ``plain'' style, a string that is used as a @tech{style name}, or a
  symbol that is used as a @tech{style name}. (Note that
  @racket[section] and @racket[para] treat symbols differently as
  @racket[style] arguments.)}


@defproc[(nested [#:style style (or/c style? string? symbol? #f) #f] 
                 [pre-flow pre-flow?] ...) nested-flow?]{

 Creates a @tech{nested flow} containing the @tech{decode}d
 @racket[pre-flow] (i.e., parsed with @racket[decode-flow]).
 
 The @racket[style] argument is handled the same as @racket[para]. The
 @racket['inset] and @racket['code-inset] styles cause the nested flow
 to be inset compared to surrounding text, with the latter
 particularly intended for insetting code. The default style is
 specified by the output destination (and tends to inset text for HTML
 output and not inset for Latex output).}


@defproc[(centered [pre-flow pre-flow?] ...) nested-flow?]{

Produces a @tech{nested flow} whose content is centered.}


@defproc[(margin-note [pre-flow pre-flow?] ...
                      [#:left? left? any/c #f])
         block?]{

Produces a @tech{nested flow} that is typeset in the margin, instead
of inlined.

If @racket[left?] is true, then the note is shown on the opposite as
it would normally be shown (which is the left-hand side for HTML
output). Beware of colliding with output for a table of contents.}


@defproc[(margin-note* [pre-content pre-content?] ...
                       [#:left? left? any/c #f]) 
         element?]{

Produces an @racket[element] that is typeset in the margin, instead of
inlined. Unlike @racket[margin-note], @racket[margin-note*] can be
used in the middle of a paragraph; at the same time, its content is
constrained to form a single paragraph in the margin.}


@defproc[(itemlist [itm items/c] ...
                   [#:style style (or/c style? string? symbol? #f) #f]) 
         itemization?]{

 Constructs an @racket[itemization] given a sequence of items. Typical
 each @racket[itm] is constructed by @racket[item], but an
 @racket[itm] can be a @tech{block} that is coerced to an
 @racket[item]. Finally, @racket[itm] can be a list or @racket[splice]
 whose elements are spliced (recursively, if necessary) into the
 @racket[itemlist] sequence.
 
 The @racket[style] argument is handled the same as @racket[para]. The
 @racket['ordered] style numbers items, instead of just using a
 bullet.}


@defthing[items/c flat-contract?]{

A contract that is equivalent to the following recursive
specification:

@racketblock[
 (or/c item? block? (listof items/c) (spliceof items/c))
]}


@defproc[(item [pre-flow pre-flow?] ...) item?]{

Creates an item for use with @racket[itemlist]. The @tech{decode}d
@racket[pre-flow] (i.e., parsed with @racket[decode-flow]) is the item
content.}


@defproc[(item? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an item produced by
@racket[item], @racket[#f] otherwise.}


@defproc[(tabular [cells (listof (listof (or/c block? content? 'cont)))]
                  [#:style style (or/c style? string? symbol? #f) #f]
                  [#:sep sep (or/c block? content? #f) #f]
                  [#:column-properties column-properties (listof any/c) '()]
                  [#:row-properties row-properties (listof any/c) '()]
                  [#:cell-properties cell-properties (listof (listof any/c)) '()])
         table?]{

Creates a @tech{table} with the given @racket[cells] content, which is
supplied as a list of rows, where each row has a list of cells. The
length of all rows must match.

Use @racket['cont] in @racket[cells] as a cell to continue the content
of the preceding cell in a row in the space that would otherwise be
used for a new cell. A @racket['cont] must not appear as the first
cell in a row.

The @racket[style] argument is handled the same as @racket[para].
See @racket[table] for a list of recognized @tech{style names} and @tech{style properties}.

If @racket[sep] is not @racket[#f], it is inserted as a new column
between every column in the table; note that any
@racket[table-columns] or @racket[table-cells] property in
@racket[style] must take the added columns into account. Otherwise,
the default style places no space between table columns. When @racket[sep]
would be placed before a @racket['cont], a @racket['cont] is inserted,
instead.

The @racket[column-properties], @racket[row-properties], and
@racket[cell-properties] arguments specify @tech{style properties} for
the columns and cells of a table; see @racket[table-columns] and
@racket[table-cells] for a description of recognized properties. The
lists do not contain entries for columns potentially introduced for
@racket[sep], and when non-empty, they are extended as needed to match
the table size determined by @racket[cells]:

@itemlist[

 @item{If the length of @racket[column-properties] is less than the
       length of each row in @racket[cells], the last item of the list
       is duplicated to make the list long enough.}

 @item{If the length of @racket[row-properties] is less than the
       length of @racket[cells], the last item of the list is
       duplicated to make the list long enough.}

 @item{If the length of @racket[cell-properties] is less than the
        number of rows in @racket[cells], then the last element is
        duplicated to make the list long enough. Each list within
        @racket[cell-properties] is treated like a
        @racket[column-properties] list---expanded as needed to match
        the number of columns in each row.}

]

Each element of @racket[column-properties] or @racket[row-properties]
is either a list of @tech{style property} values or a non-list element
that is wrapped as a list. Similarly, for each list that is an element
of @racket[cell-properties], the list's non-list elements are wrapped
as nested lists.

If @racket[column-properties] is non-empty, then its list of property
lists is converted into a @racket[table-columns] @tech{style property}
that is added to the style specified by @racket[style]---or merged
with an existing @racket[table-columns] @tech{style property} that
matches the column shape of @racket[cells]. In addition, if either
@racket[row-properties] or @racket[cell-properties] is non-empty, the
property lists of @racket[column-properties] are merged
with the property lists of @racket[row-properties] and
@racket[cell-properties]. If @racket[row-properties] or
@racket[cell-properties] is non-empty, the merged lists are
converted into a @racket[table-cells] @tech{style property} that is
added to the style specified by @racket[style]---or merged with an
existing @racket[table-cells] @tech{style property} that matches the
shape of @racket[cells].

@margin-note{If the style lists for @racket[column-properties] are
both merged with @racket[cell-properties] and converted to
@racket[table-columns], then @racket[style] will contain some
redundant information. In that case, @racket[column-attributes]
properties will be used from @racket[table-columns], while other
properties will be used from the merger into @racket[table-cells].}

@history[#:changed "1.1" @elem{Added the @racket[#:column-properties],
                               @racket[#:row-properties],
                               and @racket[#:cell-properties] arguments.}
         #:changed "1.12" @elem{Changed @racket[sep] insertion before a
                                @racket['cont].}]

Examples:
@codeblock[#:keep-lang-line? #f]|{
#lang scribble/manual
@tabular[#:sep @hspace[1]
         (list (list "soup" "gazpacho")
               (list "soup" "tonjiru"))]

@tabular[#:style 'boxed
         #:column-properties '(left right)
         #:row-properties '(bottom-border ())
         (list (list @bold{recipe}   @bold{vegetable})
               (list "caldo verde"   "kale")
               (list "kinpira gobō"  "burdock")
               (list "makizushi"     'cont))]
}|
@doc-render-examples[
  @tabular[#:sep @hspace[1]
           (list (list "soup" "gazpacho")
                 (list "soup" "tonjiru"))]

  @tabular[#:style 'boxed
           #:column-properties '(left right)
           #:row-properties '(bottom-border ())
           (list (list @bold{recipe}   @bold{vegetable})
                 (list "caldo verde"   "kale")
                 (list "kinpira gobō"  "burdock")
                 (list "makizushi"     'cont))]]
}

@defproc[(verbatim [#:indent indent exact-nonnegative-integer? 0] [elem content?] ...+)
         block?]{

Typesets string @racket[elem]s in typewriter font with linebreaks
specified by newline characters in string @racket[elem]s. Consecutive spaces in
the string @racket[elem]s are converted to @racket[hspace] to ensure that they
are all preserved in the output. Additional space (via
@racket[hspace]) as specified by @racket[indent] is added to the
beginning of each line. A non-string @racket[elem] is treated as
content within a single line.

The string @racket[elem]s are @emph{not} decoded with @racket[decode-content],
so @racket[(verbatim "---")] renders with three hyphens instead of an
em dash. Beware, however, that @emph{reading}
@litchar["@"]@racket[verbatim] converts @litchar["@"] syntax
within the argument, and such reading occurs well before
arguments to @racket[verbatim] are delivered at run-time. To disable simple
@litchar["@"] notation within the @racket[verbatim] argument,
@racket[verbatim] is typically used with
@litchar["|{"]...@litchar["}|"] or similar brackets, like this:

@verbatim[#:indent 2]|{
 @verbatim|{
   Use @bold{---} like this...
 }|
}|

which renders as

@verbatim[#:indent 2]|{
   Use @bold{---} like this...
}|

while

@verbatim[#:indent 2]||{
 @verbatim|{
   Use |@bold{---} like this...
 }|
}||

renders as

@verbatim[#:indent 2]|{
   Use |@bold{---} like this...
}|

Even with brackets like @litchar["|{"]...@litchar["}|"], beware that consistent
leading whitespace is removed by the parser; see
@secref["alt-body-syntax"] for more information.

See also @racket[literal].}

@; ------------------------------------------------------------------------

@section{Text Styles and Content}

@defproc[(elem [pre-content pre-content?] ...
               [#:style style (or/c style? string? symbol? #f) #f])
        element?]{

Wraps the @tech{decode}d @racket[pre-content] as an element with style
@racket[style].}


@def-style-proc[italic]
@def-style-proc[bold]

@defproc[(tt [pre-content pre-content?] ...) element?]{

Similar to @racket[elem], but the @racket['tt] style is used for
immediate strings and symbols among the @racket[pre-content]
arguments.

To apply the @racket['tt] style uniformly to all @racket[pre-content]
arguments, use @racket[(elem #:style 'tt pre-content ...)], instead.}

@def-style-proc[subscript]
@def-style-proc[superscript]

@def-elem-proc[smaller]{Like @racket[elem], but with style
@racket['smaller].  When uses of @racket[smaller] are nested, text
gets progressively smaller.}

@def-elem-proc[larger]{Like @racket[elem], but with style
@racket['larger].  When uses of @racket[larger] are nested, text
gets progressively larger.}

@defproc[(emph [pre-content pre-content?] ...) element?]{
The same as @racket[italic].}

@defproc[(literal [str string?] ...+) element?]{

Produces an element containing literally @racket[str]s with no
decoding via @racket[decode-content].

Beware that @litchar["@"] for a @racket[literal] call performs some
processing before delivering arguments to @racket[literal]. The
@racket[literal] form can be used with @litchar["|{"]...@litchar["}|"]
or similar brackets to disable @litchar["@"] notation within the
@racket[literal] argument, like this:

@verbatim[#:indent 2]|{
 @literal|{@bold{---}}|
}|

which renders as

@verbatim[#:indent 2]{
   @literal|{@bold{---}}|
}

See also @racket[verbatim].}

@; ------------------------------------------------------------------------
@section[#:tag "images"]{Images}


@defproc[(image [path (or/c path-string? (cons/c 'collects (listof bytes?)))]
                [#:scale scale real? 1.0]
                [#:suffixes suffixes (listof #rx"^[.]") null]
                [#:style style (or/c style? string? symbol? #f) #f]
                [pre-content pre-content?] ...)
         image-element?]{

 Creates an image element from the given path. The @tech{decode}d
 @racket[pre-content] serves as the alternate text for contexts where
 the image cannot be displayed.

 If @racket[path] is a relative path, it is relative to the current
 directory, which is set by @exec{raco setup} to
 the directory of the main document file. (In general, however, it's
 more reliable to express relative paths using
 @racket[define-runtime-path].) Instead of a path or string,
 the @racket[path] argument can be a result of
 @racket[path->main-collects-relative].

 The @racket[scale] argument sets the images scale relative to its
 default size as determined by the content of @racket[path]. For HTML
 output, the resulting @racket[image-element] is rendered with an
 @tt{img} or @tt{object} (for SVG) tag, and @racket[scale] adjusts the
 @tt{width} and @tt{height} attributes; a class name or other
 attributes in @racket[style] can effectively override that size.
 
 The strings in @racket[suffixes] are filtered to those supported by
 given renderer, and then the acceptable suffixes are tried in
 order. The HTML renderer supports @racket[".png"],
 @racket[".gif"], and @racket[".svg"], while the Latex renderer supports @racket[".png"],
 @racket[".pdf"], and @racket[".ps"] (but @racket[".ps"] works only
 when converting Latex output to DVI, and @racket[".png"] and
 @racket[".pdf"] work only for converting Latex output to PDF).
 
 Note that when the @racket[suffixes] list is non-empty, then
 the @racket[path] argument should not have a suffix.

 @history[#:changed "1.3" @elem{Added the @racket[#:style] argument.}]}

@; ------------------------------------------------------------------------
@section[#:tag "spacing"]{Spacing}

@defproc[(linebreak) element?]{
Produces an element that forces a line break.}


@def-elem-proc[nonbreaking]{Like @racket[elem], but line breaks are
suppressed while rendering the content.}


@defproc[(hspace [n exact-nonnegative-integer?]) element?]{

Produces an element containing @racket[n] spaces and style
@racket['hspace].}


@defthing[~ string?]{

A string containing the non-breaking space character,
which is equivalent to @racket['nbsp] as an element.}


@defthing[-~- string?]{

A string containing the non-breaking hyphen character.}


@defthing[?- string?]{

A string containing the soft-hyphen character (i.e., a suggestion of
where to hyphenate a word to break it across lines when rendering).}


@defthing[._ element?]{

Generates a period that ends an abbreviation in the middle of a
sentence, as opposed to a period that ends a sentence (since the
latter may be typeset with extra space). Use @litchar|{@._}| in a
document instead of just @litchar{.} for an abbreviation-ending period
that is preceded by a lowercase letter and followed by a space.

See @racket[.__] for an example.}


@defthing[.__ element?]{

Generates a period that ends a sentence (which may be typeset with
extra space), as opposed to a period that ends an abbreviation in the
middle of a sentence. Use @litchar|{@.__}| in a document instead of just
@litchar{.} for a sentence-ending period that is preceded by an
uppercase letter.

The following example illustrates both @racket[._] and @racket[.__]:

@codeblock|{
 #lang scribble/base
 My name is Mr@._ T@.__ I pity the fool who can't typeset punctuation.
}|}


@; ------------------------------------------------------------------------
@section[#:tag "base-links"]{Links}

@defproc[(hyperlink [url string?] [pre-content pre-content?] ...
                    [#:underline? underline? any/c #t]
                    [#:style style (or/c style? string? symbol? #f) (if underline? #f "plainlink")]) 
         element?]{

The @tech{decode}d @racket[pre-content] is hyperlinked to
@racket[url].  If @racket[style] is not supplied, then
@racket[underline?] determines how the link is rendered.}


@defproc[(url [dest string?]) element?]{

Generates a literal hyperlinked URL.}


@defproc[(secref [tag string?]
                 [#:doc module-path (or/c module-path? #f) #f]
                 [#:tag-prefixes prefixes (or/c (listof string?) #f) #f]
                 [#:underline? underline? any/c #t]
                 [#:link-render-style ref-style (or/c link-render-style? #f)])
         element?]{

Inserts a reference to the section tagged @racket[tag].

If @racket[#:doc module-path] is provided, the @racket[tag] refers to
a tag with a prefix determined by @racket[module-path]. When
@exec{raco setup} renders documentation, it automatically adds a tag
prefix to the document based on the source module. Thus, for example,
to refer to a section of the Racket reference,
@racket[module-path] would be @racket['(lib
"scribblings/reference/reference.scrbl")].

The @racket[#:tag-prefixes prefixes] argument similarly supports
selecting a particular section as determined by a path of tag
prefixes. When a @racket[#:doc] argument is provided, then
@racket[prefixes] should trace a path of tag-prefixed subsections to
reach the @racket[tag] section. When @racket[#:doc] is not provided,
the @racket[prefixes] path is relative to any enclosing section (i.e.,
the youngest ancestor that produces a match).

For the result @racket[link-element], if @racket[ref-style] is not
@racket[#f], then it is attached as a @tech{style property} and
affects the rendering of the link. Alternatively, an enclosing
@racket[part] can have a link-render style that adjusts the rendering
style for all links within the @tech{part}. See @racket[link-element]
for more information about the rendering of section references.

If @racket[underline?] is @racket[#f], then a @tech{style} is attached
to the result @racket[link-element] so that the hyperlink is rendered
in HTML without an underline
       
In Racket documentation that is rendered to HTML, clicking on a
section title normally shows the @racket[secref] call that is needed
to link to the section.

@history[#:changed "1.25" @elem{Added the @racket[#:link-render-style] argument.}]}


@defproc[(Secref [tag string?]
                 [#:doc module-path (or/c module-path? #f) #f]
                 [#:tag-prefixes prefixes (or/c (listof string?) #f) #f]
                 [#:underline? underline? any/c #t]
                 [#:link-render-style ref-style (or/c link-render-style? #f)])
         element?]{

Like @racket[secref], but if the rendered form of the reference starts
with a word (e.g., ``section''), then the word is capitalized.

@history[#:changed "1.25" @elem{Added the @racket[#:link-render-style] argument.}]}


@defproc[(seclink [tag string?] 
                  [#:doc module-path (or/c module-path? #f) #f]
                  [#:tag-prefixes prefixes (or/c (listof string?) #f) #f]
                  [#:underline? underline? any/c #t]
                  [#:indirect? indirect? any/c #f]
                  [pre-content pre-content?] ...) element?]{

Like @racket[secref], but the link label is the @tech{decode}d
@racket[pre-content] instead of the target section's name.

In addition to @racket[secref]'s arguments, @racket[seclink] supports
a @racket[indirect?] argument. When @racket[indirect?] is true, then
the section hyperlink's resolution in HTML is potentially delayed; see
@racket['indirect-link] for @racket[link-element].}


@defproc[(other-doc [module-path module-path?]
                    [#:underline? underline? any/c #t]
                    [#:indirect indirect (or/c #f content?) #f])
         element?]{

Like @racket[secref] for the document's implicit @racket["top"]
tag. Use this function to refer to a whole manual instead of
@racket[secref], in case a special style in the future is used for
manual titles.

If @racket[indirect] is not @racket[#f], then the link's resolution in
HTML can be delayed, like @racket[seclink] with @racket[#:indirect?
#t].  The @racket[indirect] content is prefixed with ``the'' and
suffixed with ``documentation'' to generate the rendered text of the
link. For example:

@verbatim[#:indent 2]|{
  @other-doc['(lib "parsack/parsack/parsack.scrbl")
             #:indirect "Parsec implementation in Racket"]
}|

renders as a hyperlink with the text:

@verbatim[#:indent 2]|{
  the Parsec implementation in Racket documentation
}|}


@defproc[(elemtag [t (or/c taglet? generated-tag?)] [pre-content pre-content?] ...) element?]{

The tag @racket[t] refers to the content form of
@racket[pre-content].}


@defproc[(elemref [t (or/c taglet? generated-tag?)] [pre-content pre-content?] ... 
                  [#:underline? underline? any/c #t]) element?]{

The @tech{decode}d @racket[pre-content] is hyperlinked to @racket[t],
which is normally defined using @racket[elemtag].}

@; ------------------------------------------------------------------------

@section[#:tag "base-indexing"]{Indexing}

@defproc[(index [words (or/c string? (listof string?))]
                [pre-content pre-content?] ...)
         index-element?]{

Creates an index element given a plain-text string---or list of
strings for a hierarchy, such as @racket['("strings" "plain")] for a
``plain'' entry below a more general ``strings'' entry. As index keys,
the strings are ``cleaned'' using @racket[clean-up-index-strings]. The
strings (without clean-up) also serve as the text to render in the
index. The @tech{decode}d @racket[pre-content] is the text to appear
inline as the index target.

Use @racket[index] when an index entry should point to a specific word
or phrase within the typeset document (i.e., the
@racket[pre-content]). Use @racket[section-index], instead, to create
an index entry that leads to a section, instead of a specific word or
phrase within the section.}


@defproc[(index* [words (listof string?)]
                 [word-contents (listof list?)]
                 [pre-content pre-content?] ...)
         index-element?]{
Like @racket[index], except that @racket[words] must be a list, and
the list of contents render in the index (in parallel to
@racket[words]) is supplied as @racket[word-contents].
}

@defproc[(as-index [pre-content pre-content?] ...)
         index-element?]{

Like @racket[index], but the word to index is determined by applying
@racket[content->string] on the @tech{decode}d @racket[pre-content].}


@defproc[(section-index [word string?] ...)
         part-index-decl?]{

Creates a @racket[part-index-decl] to be associated with the enclosing
section by @racket[decode]. The @racket[word]s serve as both the keys
and as the rendered forms of the keys within the index.}


@defproc[(index-section [#:tag tag (or/c #f string?) "doc-index"])
         part?]{

Produces a part that shows the index the enclosing document. The
optional @racket[tag] argument is used as the index section's tag.}


@; ------------------------------------------------------------------------

@section{Tables of Contents}

@defproc[(table-of-contents) delayed-block?]{

Returns a delayed flow element that expands to a table of contents for
the enclosing section. For Latex output, however, the table of
contents currently spans the entire enclosing document.}


@defproc[(local-table-of-contents [#:style style (or/c symbol? #f) #f])
         delayed-block?]{

Returns a delayed flow element that may expand to a table of contents
for the enclosing section, depending on the output type. For
multi-page HTML output, the flow element is a table of contents; for
Latex output, the flow element is empty.

The meaning of the @racket[style] argument depends on the output type,
but @racket['immediate-only] normally creates a table of contents that
contains only immediate sub-sections of the enclosing section. See
also the @racket['quiet] style of @racket[part] (i.e., in a
@racket[part] structure, not supplied as the @racket[style] argument
to @racket[local-table-of-contents]), which normally suppresses
sub-part entries in a table of contents.}

@; ------------------------------------------------------------------------

@section{Tags}

The exports of @racketmodname[scribble/tag] are all re-exported by
@racketmodname[scribble/base].


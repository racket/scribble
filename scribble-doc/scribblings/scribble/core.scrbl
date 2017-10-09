#lang scribble/doc
@(require scribble/manual 
          (except-in "utils.rkt" url)
          "struct-hierarchy.rkt" 
          (for-label scribble/manual-struct
                     racket/serialize
                     file/convertible
                     setup/main-collects
                     scriblib/render-cond
                     xml/xexpr
                     net/url-structs
                     scriblib/figure
                     (only-in scribble/html-render render-mixin)))

@title[#:tag "core"]{Structures And Processing}

@defmodule[scribble/core]

A document is represented as a @techlink{part}, as described in
 @secref["parts"]. This representation is intended to be
 independent of its eventual rendering, and it is intended to be
 immutable; rendering extensions and specific data in a document can
 collude arbitrarily, however.

A document is processed in four passes:

@itemlist[

 @item{The @deftech{traverse pass} traverses the document content in
       document order so that information from one part of a document
       can be communicated to other parts of the same document. The
       information is transmitted through a symbol-keyed mapping that
       can be inspected and extended by @racket[traverse-element]s and
       @racket[traverse-block]s in the document. The @tech{traverse
       pass} iterates the traversal until it obtains a fixed point
       (i.e., the mapping from one iteration is unchanged from the
       previous iteration).}

 @item{The @deftech{collect pass} globally collects information in the
       document that can span documents that are built at separate
       times, such as targets for hyperlinking.}

 @item{The @deftech{resolve pass} matches hyperlink references
       with targets and expands delayed elements (where the expansion
       should not contribute new hyperlink targets).}

 @item{The @deftech{render pass} generates the result document.}

]

None of the passes mutate the document representation. Instead, the
 @tech{traverse pass}, @tech{collect pass}, and @tech{resolve pass}
 accumulate information in a side hash table, @racket[collect-info]
 table, and @racket[resolve-info] table. The @tech{collect pass} and
 @tech{resolve pass} are effectively specialized version of
 @tech{traverse pass} that work across separately built documents.

 
@; ------------------------------------------------------------------------

@section[#:tag "parts"]{Parts, Flows, Blocks, and Paragraphs}

This diagram shows the large-scale structure of the
type hierarchy for Scribble documents. A box represents
a struct or a built-in Racket type; for example @racket[part] is a struct.
The bottom portion of a box shows the fields; for example
@racket[part] has three fields, @racket[title], @racket[blocks], 
and @racket[subparts].
The substruct relationship
is shown vertically with navy blue lines connected by a triangle;
for example, a @racket[compound-paragraph] is a @racket[block]. 
The types of values on fields are shown via dark red lines in the diagram.
Doubled lines represent lists and tripled lines represent lists
of lists; for example, the @racket[blocks] field of 
@racket[compound-paragraph] is a list of @racket[blocks].
Dotted lists represent functions that compute elements of
a given field; for example, the @racket[block] field of 
a @racket[traverse-block] struct is a function that
computes a @racket[block]. 

The diagram is not completely
accurate: a @racket[table] may have @racket['cont]
in place of a block in its @racket[cells] field, and
the types of fields are only shown if they are other structs
in the diagram.
A prose description with more detail follows the diagram.

@(mk-diagram)

A @deftech{part} is an instance of @racket[part]; among other things,
 it has a title @techlink{content}, an initial @techlink{flow}, and a
 list of subsection @techlink{parts}.  There is no difference between
 a part and a full document; a particular source module just as easily
 defines a subsection (incorporated via @racket[include-section]) as a
 document.

A @deftech{flow} is a list of @techlink{blocks}.

A @deftech{block} is either a @techlink{table}, an
 @techlink{itemization}, a @techlink{nested flow}, a
 @techlink{paragraph}, a @techlink{compound paragraph}, a
 @techlink{traverse block}, or a @techlink{delayed block}.

@itemize[

       @item{A @deftech{table} is an instance of @racket[table]; it
             has a list of list of @techlink{blocks} corresponding to
             table cells.}

       @item{A @deftech{itemization} is an instance of @racket[itemization];
             it has a list of @techlink{flows}.}

       @item{A @deftech{nested flow} is an instance of
             @racket[nested-flow]; it has a @tech{flow} that
             is typeset as sub-flow.}

       @item{A @deftech{paragraph} is an instance of
             @racket[paragraph]; it has a @tech{content}:

             @itemize[

             @item{A @deftech{content} can be a string, one of a few
                   symbols, a convertible value in the sense of @racket[convertible?],
                   an instance of @racket[element] (possibly
                   @racket[link-element], etc.), a @racket[multiarg-element], a
                   @techlink{traverse element}, a @techlink{part-relative element}, a
                   @techlink{delayed element}, or a list of content.

                   @itemize[

                   @item{A string is included in the result
                         document verbatim, except for space, and
                         unless the content's enclosing @tech{style} is
                         @racket['hspace]. In a style other than
                         @racket['hspace], consecutive spaces in the
                         output may be collapsed togther or replaced
                         with a line break. In the style
                         @racket['hspace], all text is converted to
                         uncollapsable spaces that cannot be broken
                         across lines.}

                   @item{A symbol content is either @racket['mdash],
                         @racket['ndash], @racket['ldquo],
                         @racket['lsquo], @racket['rdquo], @racket['rsquo], @racket['larr],
                         @racket['rarr], or @racket['prime]; it is
                         rendered as the corresponding HTML entity
                         (even for Latex output).}

                   @item{A convertible value in the sense of @racket[convertible?]
                         is used in a renderer-specific way, but values convertible
                         to @racket['text] renders the same as the resulting
                         string. If a renderer is not able to convert the value
                         to a known format, the value is converted to a string
                         using @racket[write].}

                   @item{An instance of @racket[element] has a
                         @techlink{content} plus a @tech{style}. The style's
                         interpretation depends on the renderer, but it
                         can be one of a few special symbols (such as
                         @racket['bold]) that are recognized by all
                         renderers.}

                   @item{An instance of @racket[link-element] has a
                         @techlink{tag} for the target of the link.}

                   @item{An instance of @racket[target-element] has a
                         @techlink{tag} to be referenced by
                         @racket[link-element]s. An instance of the
                         subtype @racket[toc-target-element] is
                         treated like a kind of section label, to be
                         shown in the ``on this page'' table for HTML
                         output.}

                   @item{An instance of @racket[index-element] has a
                         @techlink{tag} (as a target), a list of
                         strings for the keywords (for sorting and
                         search), and a list of @techlink{contents} to
                         appear in the end-of-document index.}

                   @item{An instance of @racket[image-element]
                         incorporates an image from a file into the rendered
                         document.}

                   @item{An instance of @racket[multiarg-element]
                         combines a style with a list of content,
                         where the style corresponds to a rendered
                         command that takes multiple arguments.}

                   @item{An instance of @racket[collect-element] has a
                         procedure that is called in the
                         @techlink{collect pass} of document
                         processing to record information used by
                         later passes.}

                   @item{A @deftech{traverse element} is an instance
                         of @racket[traverse-element], which
                         ultimately produces content, but can
                         accumulate and inspect information in the
                         @tech{traverse pass}.}

                   @item{A @deftech{part-relative element} is an
                         instance of @racket[part-relative-element],
                         which has a procedure that is called in the
                         @techlink{collect pass} of document
                         processing to obtain @defterm{content}. When the
                         part-relative element's procedure is called,
                         collected information is not yet available,
                         but information about the enclosing parts is
                         available.}

                   @item{A @deftech{delayed element} is an instance of
                         @racket[delayed-element], which has a
                         procedure that is called in the
                         @techlink{resolve pass} of document
                         processing to obtain @defterm{content}.}

                   @item{An instance of @racket[render-element] has a
                         procedure that is called in the
                         @techlink{render pass} of document
                         processing.}

             ]}]}

       @item{A @deftech{compound paragraph} is an instance of
             @racket[compound-paragraph]; like @racket[blockquote], it
             has list of @tech{blocks}, but the blocks are typeset as
             a single paragraph (e.g., no indentation after the first
             block) instead of inset.}

       @item{A @deftech{traverse block} is an instance of
             @racket[traverse-block], which ultimately produces
             another block, but can accumulate and inspect information
             during the @tech{traverse pass}.}

       @item{A @deftech{delayed block} is an instance of
             @racket[delayed-block], which has a procedure that
             is called in the @techlink{resolve pass} of document
             processing to obtain a @defterm{block}.}

]

@history[#:changed "1.23" @elem{Changed the handling of @racket[convertible?]
                                values to recognize a @racket['text] conversion
                                and otherwise use @racket[write].}]

@; ------------------------------------------------------------------------

@section[#:tag "tags"]{Tags}

A @deftech{tag} is a list containing a symbol and either a string, a
@racket[generated-tag] instance, or an arbitrary list. The symbol
effectively identifies the type of the tag, such as @racket['part] for
a tag that links to a part, or @racket['def] for a Racket function
definition. The symbol also effectively determines the interpretation
of the second half of the tag.

A part can have a @deftech{tag prefix}, which is effectively added
onto the second item within each tag whose first item is
@racket['part], @racket['tech], or @racket['cite], or whose second
item is a list that starts with @racket['prefixable]:

@itemlist[

 @item{The prefix is added to a string second item by creating a list
       containing the prefix and string.}

 @item{The prefix is added to a list second item after @racket['part],
       @racket['tech], or @racket['cite] using @racket[cons].}

 @item{The prefix is added to a second item that starts
       @racket['prefixable] by adding it to the list after
       @racket['prefixable].}

 @item{A prefix is not added to a  @racket[generated-tag] item.}

]

The prefix is used for reference outside the part, including the use
of tags in the part's @racket[tags] field. Typically, a document's
main part has a tag prefix that applies to the whole document;
references to sections and defined terms within the document from
other documents must include the prefix, while references within the
same document omit the prefix. Part prefixes can be used within a
document as well, to help disambiguate references within the document.

Some procedures accept a ``tag'' that is just the string part of the
full tag, where the symbol part is supplied automatically. For
example, @racket[section] and @racket[secref] both accept a string
``tag'', where @racket['part] is implicit.

The @racketmodname[scribble/tag] library provides functions for constructing
@tech{tags}.

@; ------------------------------------------------------------------------

@section[#:tag "style"]{Styles}

A @deftech{style} combines a @tech{style name} with a list of
@tech{style properties} in a @racket[style] structure. A @deftech{style name}
is either a string, symbol, or @racket[#f]. A @deftech{style property} can be
anything, including a symbol or a structure such as
@racket[color-property].

A style has a single @tech{style name}, because the name typically
corresponds to a configurable instruction to a renderer. For example,
with Latex output, a string style name corresponds to a Latex command
or environment. For more information on how string style names
interact with configuration of a renderer, see
@secref["config"]. Symbolic style names, meanwhile, provide a simple
layer of abstraction between the renderer and documents for widely
supported style; for example, the @racket['italic] style name is
supported by all renderers.

@tech{Style properties} within a style compose with style names and other
properties. Again, symbols are often used for properties that are directly
supported by renderers. For example, @racket['unnumbered] style
property for a @tech{part} renders the part without a section number.
Many properties are renderer-specific, such as a @racket[hover-property]
structure that associates text with an element to be shown in an
HTML display when the mouse hovers over the text.

@; ------------------------------------------------------------------------

@section[#:tag "passes"]{Collected and Resolved Information}

The @techlink{collect pass}, @techlink{resolve pass}, and
@techlink{render pass} processing steps all produce information that
is specific to a rendering mode. Concretely, the operations are all
represented as methods on a @racket[render<%>] object.

The result of the @method[render<%> collect] method is a
@racket[collect-info] instance. This result is provided back as an
argument to the @method[render<%> resolve] method, which produces a
@racket[resolve-info] value that encapsulates the results from both
iterations. The @racket[resolve-info] value is provided back to the
@method[render<%> resolve] method for final rendering.

Optionally, before the @method[render<%> resolve] method is called,
serialized information from other documents can be folded into the
@racket[collect-info] instance via the @method[render<%>
deserialize-info] method. Other methods provide serialized information
out of the collected and resolved records.

During the @techlink{collect pass}, the procedure associated with a
@racket[collect-element] instance can register information with
@racket[collect-put!].

During the @techlink{resolve pass}, collected information for a part
can be extracted with @racket[part-collected-info], which includes a
part's number and its parent part (or @racket[#f]). More generally,
the @racket[resolve-get] method looks up information previously
collected. This resolve-time information is normally obtained by the
procedure associated with a @techlink{delayed block} or
@techlink{delayed element}.

The @racket[resolve-get] information accepts both a @racket[part] and
a @racket[resolve-info] argument. The @racket[part] argument enables
searching for information in each enclosing part before sibling parts.

@; ------------------------------------------------------------------------

@section{Structure Reference}

@defstruct[part ([tag-prefix (or/c #f string?)]
                 [tags (listof tag?)]
                 [title-content (or/c #f list?)]
                 [style style?]
                 [to-collect list?]
                 [blocks (listof block?)]
                 [parts (listof part?)])]{

The @racket[tag-prefix] field determines the optional @techlink{tag
prefix} for the part.

The @racket[tags] indicates a list of @techlink{tags} that each link
to the section. Normally, @racket[tags] should be a non-empty list, so
that hyperlinks can target the section.

The @racket[title-content] field holds the part's title, if any.

For the @racket[style] field, the currently recognized symbolic style
names are as follows:

@itemize[

 @item{@indexed-racket['index] --- The part represents an index.}

]

The recognized @tech{style properties} are as follows:

@itemize[

 @item{@indexed-racket['unnumbered] --- A section number is not computed or
       rendered for the section.}

 @item{@indexed-racket['hidden-number] --- A section number is computed for
       the section, but it is not rendered as part of the section name.}

 @item{@indexed-racket['toc-hidden] --- The part title is not shown in tables
       of contents, including in ``on this page'' boxes. For Latex
       rendering, the part title is omitted only if it is unnumbered
       or has a hidden number.}

 @item{@indexed-racket['hidden] --- The part title is not shown; for Latex
       output, the part title is not shown only if its is empty, and
       in that case, it is also excluded from tables of contents.  The
       @racket['toc-hidden] @tech{style property} usually should be included with
       @racket['hidden] (for consistency in non-Latex output).}

 @item{@indexed-racket['grouper] --- The part is numbered with a Roman
       numeral, by default, and its subsections continue numbering as
       if they appeared in the preceeding part. In other words, the
       part acts like a ``part'' in a book where chapter numbering is
       continuous across parts.}

 @item{@tech{numberer} --- A @tech{numberer} created with
       @racket[make-numberer] determines a representation of the
       part's section number as an extension of it's patent's number.
       A @tech{numberer} overrides the default representation, which
       is a natural number or (in the case of an accompanying
       @racket['grouper] property) a Roman numeral. If a
       @racket['unnumbered] property is also present, a
       @tech{numberer} property is ignored.}

 @item{@indexed-racket['toc] --- Sub-parts of the part are rendered on separate
       pages for multi-page HTML mode.}

 @item{@indexed-racket['non-toc] --- Initial sub-parts of the part are
       @emph{not} rendered on separate pages for multi-page HTML
       mode; this @tech{style property} applies only to the main part.}

 @item{@indexed-racket['reveal] --- Shows sub-parts when this part is
       displayed in a table-of-contents panel in HTML output (which
       normally shows only the top-level sections).}

 @item{@indexed-racket['quiet] --- In HTML output and most other output modes,
       hides entries for sub-parts of this part in a
       @racket[table-of-contents] or @racket[local-table-of-contents]
       listing except when those sub-parts are top-level entries in
       the listing.}

 @item{@indexed-racket['no-toc] --- As a @tech{style property} for the main part of a
       rendered page, causes the HTML output to not include a margin box
       for the main table of contents; the ``on this page'' box that
       contains @racket[toc-element] and @racket[toc-target-element]
       links (and that only includes an ``on this page'' label for
       multi-page documents) takes on the location and color of the
       main table of contents, instead.}

 @item{@indexed-racket['no-sidebar] --- As a @tech{style property} for the main part of a
       document, causes the HTML output to not include an ``on this 
       page'' margin box.}

 @item{@indexed-racket['no-index] --- Has no effect as a @tech{style
       property} on a @racket[part], but as a style property on a
       @racket[title] or @racket[part-start] that provides a
       @racket[part]'s style via @racket[decode], the
       @racket['no-index] @tech{style property} cause @racket[decode]
       to skip the generation of an entry for the part's title in the
       document index.}

 @item{@racket[document-version] structure --- A version number for
       this part and its sub-parts (except as overridden). When it is
       not @racket[""] may be used when rendering a document; at a
       minimum, a non-@racket[""] version is rendered when it is
       attached to a part representing the whole document. The default
       version for a document is @racket[(version)]. In rendered form,
       the version is normally prefixed with the word ``Version,'' but
       this formatting can be controlled by overriding
       @tt{.version:before} and/or @tt{.versionNoNav:before} in CSS
       for HTML rendering or by redefining the @tt{\SVersionBefore}
       macro for Latex rendering (see @secref["config"]).}

 @item{@racket[document-date] structure --- A date for the part,
       normally used on a document's main part for for Latex
       output. The default date for a document is @racket[#f], which
       avoids explicitly specifying a date at the Latex level, so that
       the current date is used as the document date. Set the date to
       @racket[""] to suppress a date in an output document.}

  @item{@racket[body-id] structure --- Generated HTML uses the given
        string @tt{id} attribute of the @tt{<body>} tag; this @tech{style property} can
        be set separately for parts that start different HTML pages,
        otherwise it is effectively inherited by sub-parts; the
        default is @racket["scribble-racket-lang.org"], but
        @exec{raco setup} installs @racket["doc-racket-lang.org"] as the
        @tt{id} for any document that it builds.}

 @item{@racket[attributes] structure --- Provides additional HTML
       attributes for the @tt{<html>} tag when the part corresponds to
       its own HTML page.}

 @item{@racket[head-extra] structure --- Provides additional HTML
       content for the @tt{<head>} tag when the part corresponds to
       its own HTML page.}

 @item{@racket[color-property] structure --- For HTML, applies a color
       to the part title.}

 @item{@racket[background-color-property] structure --- For HTML,
       applies a color to the background of the part title.}

 @item{@racket[hover-property] structure --- For HTML, adds a text
       label to the title to be shown when the mouse hovers over
       it.}
           
 @item{@racket[render-convertible-as] structure --- For HTML, controls
        how objects that subscribe to the @racketmodname[file/convertible]
        protocol are rendered.}

 @item{@racket[document-source] structure --- For HTML, provides a
        module path for the part's source. Clicking on an HTML section
        title generated for the part or its sub-parts may show the
        module path plus a section-tag string, so that the user can
        create a reference to the section.}

 @item{@racket[link-render-style] structure --- Determines the default
       rendering of links to sections or other destinations within the
       section. See also @racket[link-element] and
       @racket[current-link-render-style].}

]

The @racket[to-collect] field contains @techlink{content} that is
inspected during the @techlink{collect pass}, but ignored in later
passes (i.e., it doesn't directly contribute to the output).

The @racket[blocks] field contains the part's initial flow (before
sub-parts).

The @racket[parts] field contains sub-parts.

@history[#:changed "1.25" @elem{Added @racket['no-index] support.}
         #:changed "1.26" @elem{Added @racket[link-render-style] support.}]}


@defstruct[paragraph ([style style?] [content content?])]{

A @techlink{paragraph} has a @tech{style} and a @tech{content}.

For the @racket[style] field, a string @tech{style name} corresponds
to a CSS class for HTML output or a macro for Latex output (see
@secref["extra-style"]). The following symbolic @tech{style names} are
recognized:

@itemize[

 @item{@indexed-racket['author] --- Typeset as the author of a document.  Such
       paragraphs normally should appear only in the initial flow of a
       @racket[part] for a document, where they are treated specially
       by the Latex renderer by moving the author information to the
       title.}

 @item{@indexed-racket['pretitle] --- Typeset before the title of the
       enclosing part.}

 @item{@indexed-racket['wraps] --- Like a @racket[#f] @tech{style name}, but not
       @tech{boxable} in the sense of @racket[box-mode] for Latex output.}

]

When a paragraph's style is @racket[#f], then it is @tech{boxable} in the
sense of @racket[box-mode] for Latex output.

The currently recognized @tech{style properties} are as follows:

@itemize[

 @item{@indexed-racket['omitable] --- When a table cell contains a single
       @racket[paragraph] with the @racket['omitable] @tech{style property},
       then when rendering to HTML, no @tt{<p>} tag wraps the cell
       content.}

 @item{@indexed-racket['div] --- Generates @tt{<div>} HTML output instead of
       @tt{<p>} (unless a @racket[alt-tag] property is provided).}

 @item{@racket[alt-tag] structure --- Generates the indicated HTML tag
       instead of @tt{<p>} or @tt{<div>}.}

 @item{@racket[attributes] structure --- Provides additional HTML
       attributes for the @tt{<p>}, @tt{<div>}, or alternate tag.}

 @item{@racket[body-id] structure --- For HTML, uses the given string
       as an @tt{id} attribute of the @tt{<p>}, @tt{<div>}, or
       alternate tag.}

 @item{@indexed-racket['never-indents] --- For Latex and @tech{compound
       paragraphs}; see @racket[compound-paragraph].}

 @item{@racket[box-mode] structure --- For Latex output, uses an alternate
       rendering form for @tech{boxing contexts} (such as a table cell); see
       @racket[box-mode].}

]}


@defstruct[table ([style style?]
                  [blockss (listof (listof (or/c block? 'cont)))])]{

See also the @racket[tabular] function.

A @techlink{table} has, roughly, a list of list of blocks. A cell in
the table can span multiple columns by using @racket['cont] instead of
a block in the following columns (i.e., for all but the first in a set
of cells that contain a single block).

Within @racket[style], a string @tech{style name} corresponds to a CSS
class for HTML output or an environment for Latex output (see
@secref["extra-style"]). The following symbolic style names are also
recognized:

@itemize[

 @item{@indexed-racket['boxed] --- Renders as a definition.
       This style name is not intended for use on a table that is
       nested within a @racket['boxed] table; nested uses may look
       right for some renders of the style but not others.}

 @item{@indexed-racket['centered] --- Centers HTML output horizontally.}

 @item{@indexed-racket['block] --- Prevents pages breaks in Latex output.}

]

The following @tech{style properties} are currently recognized:

@itemize[

 @item{@racket[table-columns] structure --- Provides column-specific
       styles, but only @racket[column-attributes] properties (if any)
       are used if a @racket[table-cells] structure is included as a
       @tech{style property}. See @racket[table-cells] for information
       about how a column style is used for each cell.}

 @item{@racket[table-cells] structure --- Provides cell-specific
       styles. See @racket[table-cells] for information about how the
       styles are used.}

 @item{@racket[attributes] structure --- Provides additional HTML
       attributes for the @tt{<table>} tag.}

 @item{@racket[body-id] structure --- For HTML, uses the given string
       as an @tt{id} attribute of the @tt{<table>} tag.}

 @item{@indexed-racket['aux] --- For HTML, include the table in the
       table-of-contents display for the enclosing part.}

 @item{@indexed-racket['never-indents] --- For Latex and @tech{compound
       paragraphs}; see @racket[compound-paragraph].}

]

For Latex output, a paragraph as a cell value is not automatically
line-wrapped, unless a vertical alignment is specified for the cell
through a @racket[table-cells] or @racket[table-columns]
@tech{style property}. To get a line-wrapped paragraph, use a
@racket[compound-paragraph] or use an element with a string style and
define a corresponding Latex macro in terms of @ltx{parbox}. For Latex
output of blocks in the flow that are @racket[nested-flow]s,
@racket[itemization]s, @racket[compound-paragraph]s, or
@racket[delayed-block]s, the block is wrapped with @ltxe{minipage} using
@ltx{linewidth} divided by the column count as the width.}


@defstruct[itemization ([style style?]
                        [blockss (listof (listof block?))])]{

A @techlink{itemization} has a @tech{style} and a list of @tech{flows}.

In @racket[style], a string @tech{style name} corresponds to a CSS
class for HTML output or a macro for Latex output (see
@secref["extra-style"]). In addition, the following symbolic style
names are recognized:

@itemize[

 @item{@indexed-racket['compact] --- Reduces space between items.}

 @item{@indexed-racket['ordered] --- Generates @tt{<ol>} HTML output instead
       of @tt{<ul>} or an Latex enumeration instead of an
       itemization.}
]

The following @tech{style properties} are currently recognized:

@itemize[

 @item{@racket[attributes] structure --- Provides additional HTML
       attributes for the @tt{<ul>} or @tt{<ol>} tag.}

 @item{@racket[body-id] structure --- For HTML, uses the given string
       as an @tt{id} attribute of the @tt{<ul>} or @tt{<ol>} tag.}

 @item{@indexed-racket['never-indents] --- For Latex and @tech{compound
       paragraphs}; see @racket[compound-paragraph].}

]}


@defstruct[nested-flow ([style any/c]
                        [blocks (listof block?)])]{

A @techlink{nested flow} has a style and a @tech{flow}.

In @racket[style], the @tech{style name} is normally a string that
corresponds to a CSS class for HTML @tt{<blockquote>} output or a Latex
environment (see @secref["extra-style"]). The following symbolic style
names are recognized:

@itemize[

 @item{@indexed-racket['inset] --- Insets the nested flow relative to
       surrounding text.}

 @item{@indexed-racket['code-inset] --- Insets the nested flow relative to
       surrounding text in a way suitable for code. If the nested flow
       has a single block, then it is @tech{boxable} in the sense of
       @racket[box-mode] for Latex output.}

 @item{@indexed-racket['vertical-inset] --- Insets the nested flow vertically
       relative to surrounding text, but not horizontally. If the
       nested flow has a single block, then it is @tech{boxable} in the sense
       of @racket[box-mode] for Latex output.}

]

The following @tech{style properties} are currently recognized:

@itemize[

 @item{@indexed-racket['command] --- For Latex output, a string @tech{style
       name} is used as a command name instead of an environment
       name.}

 @item{@indexed-racket['multicommand] --- For Latex output, a string
       @tech{style name} is used as a command name with a separate
       argument for each block in @racket[blocks].}

 @item{@racket[attributes] structure --- Provides additional HTML
       attributes for the @tt{<blockquote>} tag.}

 @item{@racket[body-id] structure --- For HTML, uses the given string
       as an @tt{id} attribute of the @tt{<blockquote>} tag.}

 @item{@indexed-racket['never-indents] --- For Latex and @tech{compound
       paragraphs}; see @racket[compound-paragraph].}

 @item{@racket[box-mode] structure --- For Latex output, uses an alternate
       rendering form for @tech{boxing contexts} (such as a table cell); see
       @racket[box-mode].}

 @item{@indexed-racket['decorative] --- The content of the nested flow is intended
       for decoration. Text output skips a decorative nested flow.}

 @item{@racket[alt-tag] structure --- Generates the indicated HTML tag
       instead of @tt{<blockquote>}.}

 @item{@indexed-racket['pretitle] --- For Latex, raises the contents
   of the flow to above the title.}
]}


@defstruct[compound-paragraph ([style style?]
                               [blocks (listof block?)])]{

A @techlink{compound paragraph} has a @tech{style} and a list of
@tech{blocks}.

For HTML, a @racket[paragraph] block in @racket[blocks] is rendered
without a @tt{<p>} tag, unless the paragraph has a style with a
non-@racket[#f] @tech{style name}. For Latex, each @tech{block} in
@racket[blocks] is rendered with a preceding @ltx{noindent}, unless
the block has the @racket['never-indents] property (checking
recursively in a @racket[nested-flow] or @racket[compound-paragraph]
if the @racket[nested-flow] or @racket[compound-paragraph] itself has
no @racket['never-indents] property).

The @racket[style] field of a compound paragraph is normally a string
that corresponds to a CSS class for HTML output or Latex environment
for Latex output (see @secref["extra-style"]). The following
@tech{style properties} are currently recognized:

@itemize[

 @item{@indexed-racket['command] --- For Latex output, a string @tech{style
       name} is used as a command name instead of an environment
       name.}

 @item{@racket[alt-tag] structure --- Generates the given HTML tag
       instead of @tt{<p>}.}

 @item{@racket[attributes] structure --- Provides additional HTML
       attributes for the @tt{<p>} or alternate tag.}

 @item{@racket[body-id] structure --- For HTML, uses the given string
       as an @tt{id} attribute of the @tt{<p>} or alternate tag.}

 @item{@indexed-racket['never-indents] --- For Latex within another
       @tech{compound paragraph}; see above.}

]}


@defstruct[traverse-block ([traverse block-traverse-procedure/c])]{

Produces another block during the @tech{traverse pass}, eventually.

The @racket[traverse] procedure is called with @racket[_get] and
@racket[_set] procedures to get and set symbol-keyed information; the
@racket[traverse] procedure should return either a @tech{block} (which
effectively takes the @racket[traverse-block]'s place) or a procedure
like @racket[traverse] to be called in the next iteration of the
@tech{traverse pass}.

All @racket[traverse-element] and @racket[traverse-block]s that have
not been replaced are forced in document order relative to each other
during an iteration of the @tech{traverse pass}.

The @racket[_get] procedure passed to @racket[traverse] takes a symbol
and any value to act as a default; it returns information registered
for the symbol or the given default if no value has been
registered. The @racket[_set] procedure passed to @racket[traverse]
takes a symbol and a value to registered for the symbol.

@margin-note*{See also @racket[cond-block] in @racketmodname[scriblib/render-cond].}
@;
The symbol @indexed-racket['scribble:current-render-mode] is
automatically registered to a list of symbols that describe the
target of document rendering. The list contains @racket['html]
when rendering to HTML, @racket['latex] when rendering via Latex, and
@racket['text] when rendering to text. The registration of
@racket['scribble:current-render-mode] cannot be changed via
@racket[_set].}


@defstruct[delayed-block ([resolve (any/c part? resolve-info? . -> . block?)])]{

The @racket[resolve] procedure is called during the @techlink{resolve
pass} to obtain a normal @tech{block}. The first argument to
@racket[resolve] is the renderer.

}


@defstruct[element ([style element-style?]
                    [content content?])]{

Styled @tech{content} within an enclosing @tech{paragraph} or other content.

The @racket[style] field can be a @racket[style] structure, but it can
also be just a @tech{style name}.

In @racket[style], a string @tech{style name} corresponds to a CSS
class for HTML output and a macro name for Latex output (see
@secref["extra-style"]). The following symbolic style names are
recognized:

@itemize[

 @item{@indexed-racket['tt], @racket['italic], @racket['bold], @racket['roman], @racket['sf],
       @racket['url], @racket['subscript], @racket['superscript], 
       @racket['smaller], @racket['larger] ---
       Basic styles recognized by all renders.}

 @item{@indexed-racket['hspace] --- Renders its @racket[content] as monospace
       blanks.}
 
 @item{@indexed-racket['newline] --- Renders a line break independent of
       the @racket[content].}

 @item{@indexed-racket['no-break] --- Prevents line breaks when rendering
       @racket[content].}

]

The following @tech{style properties} are currently recognized:

@itemize[

 @item{@racket[target-url] structure --- Generates a hyperlink.}

 @item{@racket[url-anchor] structure --- For HTML, inserts a hyperlink
       target before @racket[content].}

 @item{@racket[color-property] structure --- Applies a color to the
       text of @racket[content].}

 @item{@racket[background-color-property] structure --- Applies a
       color to the background of @racket[content].}

 @item{@racket[alt-tag] structure --- Generates the given HTML tag
       instead of the default one (@tt{<span>}, @tt{<b>}, @|etc|).}

 @item{@racket[attributes] structure --- Provides additional HTML
       attributes for a tag.}

 @item{@racket[hover-property] structure --- For HTML, adds a text
       label to the content to be shown when the mouse hovers over
       it.}

 @item{@racket[script-property] structure --- For HTML, supplies a
       script alternative to @racket[content].}

  @item{@racket[body-id] structure --- For HTML uses the given
        string as an @tt{id} attribute of the @tt{<span>} tag.}

  @item{@indexed-racket['aux] --- Intended for use in titles, where the
        auxiliary part of the title can be omitted in hyperlinks. See,
        for example, @racket[secref].}

  @item{@indexed-racket['tt-chars] --- For Latex output, when the @tech{style
        name} is a string, render the element's content with escapes
        suitable for Latex @tt{tt} mode.}

  @item{@indexed-racket['exact-chars] --- For Latex output, when the @tech{style
        name} is a string or @racket[#f], render the elements content exactly
        (without escapes).}

  @item{@racket[command-extras] structure --- For Latex output,
         adds strings as arguments to the Latex command.}

]

@history[#:changed "1.6" @elem{Changed @racket['exact-chars] handling to
         take effect when the style name is @racket[#f].}]}


@defstruct[(image-element element) ([path (or/c path-string?
                                                (cons/c 'collects (listof bytes?)))]
                                    [suffixes (listof #rx"^[.]")]
                                    [scale real?])]{

Used as a style for an @racket[element] to inline an image. The
@racket[path] field can be a result of
@racket[path->main-collects-relative].

For each string in @racket[suffixes], if the rendered works with the
corresponding suffix, the suffix is added to @racket[path] and used if
the resulting path refers to a file that exists. The order in
@racket[suffixes] determines the order in which suffixes are
tried. The HTML renderer supports @racket[".png"], @racket[".gif"], and @racket[".svg"],
while the Latex renderer supports @racket[".png"], @racket[".pdf"],
and @racket[".ps"] (but rendering Latex output to PDF will not work
with @racket[".ps"] files, while rendering to Latex DVI output works
only with @racket[".ps"] files). If @racket[suffixes] is empty or if
none of the suffixes lead to files that exist, @racket[path] is used
as-is.

The @racket[scale] field scales the image in its rendered form.}


@defstruct[(target-element element) ([_tag tag?])]{

Declares the content as a hyperlink target for @racket[_tag].}


@defstruct[(toc-target-element target-element) ()]{

Like @racket[target-element], the content is also a kind of section
label to be shown in the ``on this page'' table for HTML output.}


@defstruct[(toc-target2-element toc-target-element) ([toc-content content?])]{

Extends @racket[target-element] with a separate field for the content
to be shown in the ``on this page'' table for HTML output.}


@defstruct[(page-target-element target-element) ()]{

Like @racket[target-element], but a link to the element goes to the
top of the containing page.}


@defstruct[(redirect-target-element target-element) ([alt-path path-string?]
                                                     [alt-anchor string?])]{

Like @racket[target-element], but a link to the element is redirected
to the given URL.}


@defstruct[(toc-element element) ([toc-content content?])]{

Similar to @racket[toc-target-element], but with specific content for
the ``on this page'' table specified in the @racket[toc-content]
field.}


@defstruct[(link-element element) ([tag tag?])]{

Represents a hyperlinks to @racket[_tag].

Normally, the content of the element is rendered as the hyperlink.
When @racket[_tag] is a part tag and the content of the element is
@racket[null], however, rendering is treated specially based on the
@racket[_mode] value of a @racket[link-render-style] @tech{style
property}:

@itemlist[

 @item{For HTML output, in the @racket['default] mode, the generated
       reference is the hyperlinked title of the elements in the
       section's title content, except that elements with the
       @racket['aux] @tech{style property} are omitted in the
       hyperlink label.

       In @racket['number] mode, the section title is not shown.
       Instead, the word ``section'' is shown followed by a
       hyperlinked section number. The word ``section'' starts in
       uppercase if the element's style includes a @racket['uppercase]
       property.}

 @item{For Latex/PDF output, the generated reference's format can
       depend on the document style in addition the @racket[_mode].
       For the @racket['default] mode and a default document style, a
       section number is shown by the word ``section'' followed by the
       section number, and the word ``section'' and the section number
       are together hyperlinked. The word ``section'' starts in
       uppercase if the element's style includes a @racket['uppercase]
       property. The @racketmodname[scribble/manual] style uses the
       symbol ``§'' in place of the word ``section''.

       In @racket['number] mode, rendering is the same, except that
       only the number is hyperlinked, not the word ``section'' or
       the ``§'' symbol.

       A new document style can customize Latex/PDF output (see
       @secref["config"]) by redefining the @ltx{SecRefLocal}, @|etc|,
       macros (see @secref["builtin-latex"]). The @ltx{SecRef},
       @|etc|, variants are used in @racket['number] mode.}

]

If a @racket[link-render-style] @tech{style property} is not attached
to a @racket[link-element] that refers to a part, a
@racket[link-render-style] @tech{style property} that is attached to
an enclosing part is used, since attaching a
@racket[link-render-style] @tech{style property} to a part causes
@racket[current-link-render-style] to be set while rendering the part.
Otherwise, the render-time value of @racket[current-link-render-style]
determine's a @racket[link-element]'s rendering.

The following style properties are recognized in addition to the style
properties for all @racket[element]s:

@itemize[

 @item{@racket[link-render-style] structure --- As described above.}

 @item{@indexed-racket['indirect-link] --- For HTML output, treats the link as
       ``external''. When rendering to HTML and the
       @method[render-mixin set-external-tag-path] method is called to
       provide an external-link URL, then the resolution of the
       hyperlink can be deferred until the link is clicked (or, in
       some cases, patched by JavaScript when the documentation is
       viewed in a browser).}

]

@history[#:changed "1.26" @elem{Added @racket[link-render-style] support.}]}


@defstruct[(index-element element) ([tag tag?]
                                    [plain-seq (and/c pair? (listof string?))]
                                    [entry-seq (listof content?)]
                                    [desc any/c])]{

The @racket[plain-seq] specifies the keys for sorting, where the first
string is the main key, the second is a sub-key, etc. For
example, an ``night'' portion of an index might have sub-entries for
``night, things that go bump in'' and ``night, defender of the''. The
former would be represented by @racket[plain-seq] @racket['("night"
"things that go bump in")], and the latter by @racket['("night"
"defender of the")]. Naturally, single-string
@racket[plain-seq] lists are the common case, and at least one word is
required, but there is no limit to the word-list length. The strings in 
@racket[plain-seq] must not contain a newline character.

The @racket[entry-seq] list must have the same length as
@racket[plain-seq]. It provides the form of each key to render in the
final document.

The @racket[desc] field provides additional information about the
index entry as supplied by the entry creator. For example, a reference
to a procedure binding can be recognized when @racket[desc] is an
instance of @racket[procedure-index-desc]. See
@racketmodname[scribble/manual-struct] for other typical types of
@racket[desc] values.

See also @racket[index].}


@defstruct[multiarg-element ([style element-style?]
                             [contents (listof content?)])]{

Like @racket[element] with a list for content, except that for Latex
output, if the @tech{style name} in @racket[style] is a string, then
it corresponds to a Latex command that accepts as many arguments (each
in curly braces) as elements of @racket[contents].}


@defstruct[traverse-element ([traverse element-traverse-procedure/c])]{

@margin-note*{See also @racket[cond-element] in @racketmodname[scriblib/render-cond].}
@;
Like @racket[traverse-block], but the @racket[traverse] procedure must
eventually produce @tech{content}, rather than a @tech{block}.}


@defstruct[delayed-element ([resolve (any/c part? resolve-info? . -> . content?)]
                            [sizer (-> any/c)]
                            [plain (-> any/c)])]{

The @racket[render] procedure's arguments are the same as for
@racket[delayed-block], but the result is @techlink{content}. 
Unlike @racket[delayed-block], the
result of the @racket[render] procedure's argument is remembered on
the first call for re-use for a particular resolve pass.

The @racket[sizer] field is a procedure that produces a substitute
@techlink{content} for the delayed element for the purposes of
determining the delayed element's width (see @racket[element-width]).

The @racket[plain] field is a procedure that produces a substitute
@techlink{content} when needed before the @techlink{collect pass},
such as when @racket[element->string] is used before the @tech{collect
pass}.}


@defstruct[part-relative-element ([resolve (collect-info? . -> . content?)]
                                  [sizer (-> any/c)]
                                  [plain (-> any/c)])]{

Similar to @racket[delayed-block], but the replacement
@techlink{content} is obtained in the @techlink{collect pass} by
calling the function in the @racket[resolve] field.

The @racket[resolve] function can call @racket[collect-info-parents]
to obtain a list of @techlink{parts} that enclose the element,
starting with the nearest enclosing section. Functions like
@racket[part-collected-info] and @racket[collected-info-number] can
extract information like the part number.}


@defstruct[(collect-element element) ([collect (collect-info . -> . any)])]{

Like @racket[element], but the @racket[collect] procedure is called
during the @techlink{collect pass}. The @racket[collect] procedure
normally calls @racket[collect-put!].

Unlike @racket[delayed-element] or @racket[part-relative-element], the
element remains intact (i.e., it is not replaced) by either the
@tech{collect pass} or @tech{resolve pass}.}


@defstruct[(render-element element) ([render (any/c part? resolve-info? . -> . any)])]{

Like @racket[delayed-element], but the @racket[render] procedure is called
during the @techlink{render pass}.

If a @racket[render-element] instance is serialized (such as when
saving collected info), it is reduced to a @racket[element] instance.}


@defstruct[collected-info ([number (listof part-number-item?)]
                           [parent (or/c #f part?)]
                           [info any/c])]{

Computed for each part by the @techlink{collect pass}.

The length of the @racket[number] list indicates the section's nesting
depth. Elements of @racket[number] correspond to the section's number,
it's parent's number, and so on (that is, the section numbers are in
reverse order):

@itemlist[

 @item{A number value corresponds to a normally numbered
       section.}

 @item{A non-empty string corresponds to a @racket['grouper] section,
       which is shown as part of the combined section number only when
       it's the first element.}

 @item{A a list corresponds to a @tech{numberer}-generated section
       string plus its separator string, where the separator is used
       in a combined section number after the section string and
       before a subsection's number (or, for some output modes, before
       the title of the section).}

 @item{For an unnumbered section, a @racket[#f] is used in place of
       any number or lists element, while @racket[""] is used in place
       of all non-empty strings.}

]

@history[#:changed "1.1" @elem{Added @racket[(list/c string? string?)]
                               number items for
                               @tech{numberer}-generated section
                               numbers.}]}


@defstruct[target-url ([addr path-string?])]{

Used as a @tech{style property} for an @racket[element]. A path is
allowed for @racket[addr], but a string is interpreted as a URL rather
than a file path.}


@defstruct[document-version ([text (or/c string? #f)])]{

Used as a @tech{style property} for a @racket[part] to indicate a
version number.}


@defstruct[document-date ([text (or/c string? #f)])]{

Used as a @tech{style property} for a @racket[part] to indicate a
date (which is typically used for Latex output).}


@defstruct[color-property ([color (or/c string? (list/c byte? byte? byte?))])]{

Used as a @tech{style property} for an @racket[element] to set its
color. Recognized string names for @racket[color] depend on the
renderer, but at the recognized set includes at least
@racket["white"], @racket["black"], @racket["red"], @racket["green"],
@racket["blue"], @racket["cyan"], @racket["magenta"], and
@racket["yellow"]. When @racket[color] is a list of bytes, the values
are used as RGB levels.

When rendering to HTML, a @racket[color-property] is also recognized
for a @tech{block}, @racket[part] (and used for the title in the
latter case)or cell in a @racket[table].}


@defstruct[background-color-property ([color (or/c string? (list/c byte? byte? byte?))])]{

Like @racket[color-property], but sets the background color.}


@defstruct[table-cells ([styless (listof (listof style?))])]{

Used as a @tech{style property} for a @racket[table] to set its cells'
styles.

If a cell style has a string name, it is used as an HTML class for the
@tt{<td>} tag or as a Latex command name.

The following are recognized as cell-@tech{style properties}:

@itemize[

 @item{@indexed-racket['left] --- Left-align the cell content.}

 @item{@indexed-racket['right] --- Right-align the cell content top baselines.}

 @item{@indexed-racket['center] --- Center the cell content horizontally.}

 @item{@indexed-racket['top] --- Top-align the cell content.}

 @item{@indexed-racket['baseline] --- Align the cell content top baselines.}

 @item{@indexed-racket['bottom] --- bottom-align the cell content.}

 @item{@indexed-racket['vcenter] --- Center the cell content vertically.}

 @item{@indexed-racket['border] --- Draw a line around all sides of the
       cell. Borders along a shared edge of adjacent cells are
       collapsed into a single line.}

 @item{@indexed-racket['left-border], @indexed-racket['right-border],
       @indexed-racket['top-border], or @indexed-racket['bottom-border] --- Draw a
       line along the corresponding side of the cell (with the same
       border collapsing as for @racket['border]).}

 @item{@racket[color-property] structure --- For HTML, applies a color
       to the cell content.}

 @item{@racket[background-color-property] structure --- For HTML,
       applies a color to the background of the cell.}

 @item{@racket[attributes] --- Provides additional HTML attributes
       for the cell's @tt{<td>} tag.}

]

@history[#:changed "1.1" @elem{Added @racket[color-property] and 
                               @racket[background-color-property] support.}
         #:changed "1.4" @elem{Added @racket['border], @racket['left-border],
                               @racket['right-border], @racket['top-border],
                               and @racket['bottom-border] support.}]}


@defstruct[table-columns ([styles (listof style?)])]{

Like @racket[table-cells], but with support for a
@racket[column-attributes] property in each style, and the
@racket[styles] list is otherwise duplicated for each row in the
table. The non-@racket[column-attributes] parts of a
@racket[table-columns] are used only when a @racket[table-cells] property is
not present along with the @racket[table-columns] property.

For HTML table rendering, for each column that has a
@racket[column-attributes] property in the corresponding element of
@racket[styles], the attributes are put into an HTML @tt{col} tag
within the table.}


@deftogether[(
@defstruct[box-mode ([top-name string?]
                     [center-name string?]
                     [bottom-name string?])]
@defproc[(box-mode* [name string?]) box-mode?]
)]{

As a @tech{style property}, indicates that a @tech{nested flow} or
@tech{paragraph} is @deftech{boxable} when it is used in a
@deftech{boxing context} for Latex output, but a @tech{nested flow} is
@tech{boxable} only if its content is also @tech{boxable}.

A @tech{boxing context} starts with a table cell in a multi-column
table, and the content of a @tech{block} in a @tech{boxing context} is
also in a @tech{boxing context}. If the cell's content is
@tech{boxable}, then the content determines the width of the cell,
otherwise a width is imposed. A @tech{paragraph} with a @racket[#f]
@tech{style name} is @tech{boxable} as a single line; the
@racket['wraps] @tech{style name} makes the paragraph
non-@tech{boxable} so that its width is imposed and its content can
use multiple lines. A @tech{table} is @tech{boxable} when that all of
its cell content is boxable.

To generate output in box mode, the @racket[box-mode] property
supplies Latex macro names to apply to the @tech{nested flow} or
@tech{paragraph} content. The @racket[top-name] macro is used if the
box's top line is to be aligned with other boxes, @racket[center-name]
if the box's center is to be aligned, and @racket[bottom-name] if the
box's bottom line is to be aligned. The @racket[box-mode*] function
creates a @racket[box-mode] structure with the same name for all three
fields.

A @racket[box-mode] @tech{style property} overrides any automatic
boxed rendering (e.g., for a @tech{paragraph} with @tech{style name}
@racket[#f]).  If a @tech{block} has both a @racket[box-mode]
@tech{style property} and a @racket['multicommand] @tech{style
property}, then the Latex macro @racket[top-name],
@racket[center-name], or @racket[bottom-name] is applied with a
separate argument for each of its content.}


@defproc[(block? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @racket[paragraph],
@racket[table], @racket[itemization], @racket[nested-flow],
@racket[traverse-block], or @racket[delayed-block], @racket[#f]
otherwise.}


@defproc[(content? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a string, symbol,
@racket[element], @racket[multiarg-element],
@racket[traverse-element], @racket[delayed-element],
@racket[part-relative-element], a convertible value in 
the sense of @racket[convertible?], or list of @tech{content}. 
Otherwise, it returns @racket[#f].}


@defstruct[style ([name (or/c string? symbol? #f)]
                  [properties list?])]{

Represents a @techlink{style}.}


@defthing[plain style?]{

A style @racket[(make-style #f null)].}


@defproc[(element-style? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a string, symbol, @racket[#f],
or @racket[style] structure.}


@defproc[(tag? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is acceptable as a link
@techlink{tag}, which is a list containing a symbol and either a
string, a @racket[generated-tag] instance, or a non-empty list
of @racket[serializable?] values.}


@defstruct[generated-tag ()]{

A placeholder for a tag to be generated during the @techlink{collect
 pass}. Use @racket[tag-key] to convert a tag containing a
 @racket[generated-tag] instance to one containing a string.

}


@defproc*[([(content->string (content content?)) string?]
           [(content->string (content content?) (renderer any/c) (p part?) (info resolve-info?)) string?])]{

Converts @tech{content} to a single string (essentially
rendering the content as ``plain text'').

If @racket[p] and @racket[info] arguments are not supplied, then a
pre-``collect'' substitute is obtained for @tech{delayed
elements}. Otherwise, the two arguments are used to force the
@tech{delayed element} (if it has not been forced already).}

@defproc[(content-width [c content?]) exact-nonnegative-integer?]{

Returns the width in characters of the given @tech{content}.

}


@defproc[(block-width (e block?)) exact-nonnegative-integer?]{

Returns the width in characters of the given @tech{block}.}


@defproc[(part-number-item? [v any/c]) boolean]{

Return @racket[#t] if @racket[v] is @racket[#f], an exact non-negative
integer, a string, or a list containing two strings. See @racket[collected-info]
for information on how different representations are used for numbering.

@history[#:added "1.1"]}


@deftogether[(
@defproc[(numberer? [v any/c]) boolean?]
@defproc[(make-numberer [step (any/c (listof part-number-item?)
                               . -> .
                               (values part-number-item? any/c))]
                        [initial-value any/c])
          numberer?]
@defproc[(numberer-step [n numberer?]
                        [parent-number (listof part-number-item?)]
                        [ci collect-info?]
                        [numberer-values hash?])
         (values part-number-item? hash?)]
)]{

A @deftech{numberer} implements a representation of a section number
that increment separately from the default numbering style and that
can be rendered differently than as Arabic numerals.

The @racket[numberer?] function returns @racket[#t] if @racket[v] is a
@tech{numberer}, or @racket[#f] otherwise.

The @racket[make-numberer] function creates a @tech{numberer}. The
@racket[step] function computes both the current number's
representation and increments the number, where the ``number'' can be
an arbitrary value; the @racket[initial-value] argument determines the
initial value of the ``number'', and the @racket[step] function
receives the current value as its first argument and returns an
incremented value as its second result. A numberer's ``number'' value
starts fresh at each new nesting level. In addition to the numberer's
current value, the @racket[step] function receives the parent
section's numbering (so that its result can depend on the part's
nesting depth).

The @racket[numberer-step] function is normally used by a renderer. It
applies a @tech{numberer}, given the parent section's number, a
@racket[collect-info] value, and a hash table that accumulates
@tech{numberer} values at a given nesting layer. The
@racket[collect-info] argument is needed because a @tech{numberer}'s
identity is based on a @racket[generated-tag]. The result of
@racket[numberer-step] is the rendered form of the current section
number plus an updated hash table with an incremented value for the
@tech{numberer}.

Typically, the rendered form of a section number (produced by
@racket[numberer-step]) is a list containing two strings. The first
string is the part's immediate number, which can be combined with a
prefix for enclosing parts' numbers. The second string is a separator
that is placed after the part's number and before a subsection's
number for each subsection. If @racket[numberer-step] produces a plain
string for the rendered number, then it is not added as a prefix to
subsection numbers. See also @racket[collected-info].

@history[#:added "1.1"]}


@defstruct[link-render-style ([mode (or/c 'default 'number)])]{

Used as a @tech{style property} for a @racket[part] or a specific
@racket[link-element] to control the way that a hyperlink is rendered
for a part via @racket[secref] or for a figure via @racket[figure-ref]
from @racketmodname[scriblib/figure].

The @racket['default] and @racket['number] modes represent generic
hyperlink-style configurations that could make sense for various kinds
of references. The @racket['number] style is intended to mean that a
specific number is shown for the reference and that only the number is
hyperlinked. The @racket['default] style is more flexible, allowing a
more appropriate choice for the rendering context, such as using the
target section's name for a hyperlink in HTML.

@history[#:added "1.26"]}


@defparam[current-link-render-style style link-render-style?]{

A parameter that determines the default rendering style for a section
link.

When a @racket[part] has a @racket[link-render-style] as one of its
@tech{style properties}, then the @racket[current-link-render-style]
parameter is set during the @tech{resolve pass} and @tech{render pass}
for the @racket[part]'s content.

@history[#:added "1.26"]}


@defstruct[collect-info ([fp any/c] [ht any/c] [ext-ht any/c] 
                         [ext-demand (tag? collect-info? . -> . any/c)]
                         [parts any/c] 
                         [tags any/c] [gen-prefix any/c] 
                         [relatives any/c] 
                         [parents (listof part?)])]{

Encapsulates information accumulated (or being accumulated) from the
@techlink{collect pass}. The fields are exposed, but not currently
intended for external use, except that @racket[collect-info-parents]
is intended for external use.

}

@defstruct[resolve-info ([ci any/c] [delays any/c] [undef any/c] [searches any/c])]{

Encapsulates information accumulated (or being accumulated) from the
@techlink{resolve pass}. The fields are exposed, but not currently
intended for external use.

}

@defproc[(info-key? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an @deftech{info key}: a list of
at least two elements whose first element is a symbol. The result is
@racket[#f] otherwise.

For a list that is an info tag, the interpretation of the second
element of the list is effectively determined by the leading symbol,
which classifies the key. However, a @racket[#f] value as the second
element has an extra meaning: collected information mapped by such
info keys is not propagated out of the part where it is collected;
that is, the information is available within the part and its
sub-parts, but not in ancestor or sibling parts.

Note that every @techlink{tag} is an info key.

}

@defproc[(collect-put! [ci collect-info?] [key info-key?] [val any/c])
         void?]{

Registers information in @racket[ci]. This procedure should be called
only during the @techlink{collect pass}.

}

@defproc[(resolve-get [p (or/c part? #f)] [ri resolve-info?] [key info-key?])
         any/c]{

Extract information during the @techlink{resolve pass} or
@techlink{render pass} for @racket[p] from @racket[ri], where the
information was previously registered during the @techlink{collect
pass}. See also @secref["passes"].

The result is @racket[#f] if the no value for the given key is found.
Furthermore, the search failure is recorded for potential consistency
reporting, such as when @exec{racket setup} is used to build
documentation.

}


@defproc[(resolve-get/ext? [p (or/c part? #f)] [ri resolve-info?] [key info-key?])
         (values any/c boolean?)]{

Like @racket[resolve-get], but returns a second value to indicate
whether the resulting information originated from an external source
(i.e., a different document).}


@defproc[(resolve-get/ext-id [p (or/c part? #f)] [ri resolve-info?] [key info-key?])
         (values any/c (or/c boolean? string?))]{

Like @racket[resolve-get/ext?], but the second result can be a string
to indicate the source document's identification as established via
@racket[load-xref] and a @racket[#:doc-id] argument.

@history[#:added "1.1"]}


@defproc[(resolve-search [dep-key any/c] [p (or/c part? #f)] [ri resolve-info?] [key info-key?])
         void?]{

Like @racket[resolve-get], but a shared @racket[dep-key] groups
multiple searches as a single request for the purposes of consistency
reporting and dependency tracking. That is, a single success for the
same @racket[dep-key] means that all of the failed attempts for the
same @racket[dep-key] have been satisfied. However, for dependency
checking, such as when using @exec{racket setup} to re-build
documentation, all attempts are recorded (in case external changes
mean that an earlier attempt would succeed next time).

}

@defproc[(resolve-get/tentative [p (or/c part? #f)] [ri resolve-info?] [key info-key?])
         any/c]{

Like @racket[resolve-search], but without dependency tracking. For
multi-document settings where dependencies are normally tracked, such
as when using @exec{racket setup} to build documentation, this function
is suitable for use only for information within a single document.

}

@defproc[(resolve-get-keys [p (or/c part? #f)]
                           [ri resolve-info?] 
                           [pred (info-key? . -> . any/c)])
         list?]{

Applies @racket[pred] to each key mapped for @racket[p] in
@racket[ri], returning a list of all keys for which @racket[pred]
returns a true value.

}

@defproc[(part-collected-info [p part?]
                              [ri resolve-info?])
         collected-info?]{

Returns the information collected for @racket[p] as recorded within
@racket[ri].

}

@defproc[(tag-key [t tag?] [ri resolve-info?]) tag?]{

Converts a @racket[generated-tag] value with @racket[t] to a string.

}


@defproc[(traverse-block-block [b traverse-block?]
                               [i (or/c resolve-info? collect-info?)])
         block?]{

Produces the block that replaces @racket[b].}


@defproc[(traverse-element-content [e traverse-element?]
                                   [i (or/c resolve-info? collect-info?)])
         content?]{

Produces the content that replaces @racket[e].}


@defthing[block-traverse-procedure/c contract?]{

Defined as

@racketblock[
  (recursive-contract
   ((symbol? any/c . -> . any/c)
    (symbol? any/c . -> . any)
    . -> . (or/c block-traverse-procedure/c
                 block?)))
]}

@defthing[element-traverse-procedure/c contract?]{

Defined as

@racketblock[
  (recursive-contract
   ((symbol? any/c . -> . any/c)
    (symbol? any/c . -> . any)
    . -> . (or/c element-traverse-procedure/c
                 content?)))
]}

@; ----------------------------------------

@section{HTML Style Properties}

@defmodule[scribble/html-properties]{ The
@racket[scribble/html-properties] library provides datatypes used as
@tech{style properties} for HTML rendering.}


@defstruct[attributes ([assoc (listof (cons/c symbol? string?))])]{

Used as a @tech{style property} to add arbitrary attributes to an HTML
tag.}


@defstruct[alt-tag ([name (and/c string? #rx"^[a-zA-Z0-9]+$")])]{

Use as a @tech{style property} for an @racket[element],
@racket[paragraph], or @racket[compound-paragraph] to substitute an
alternate HTML tag (instead of @tt{<span>}, @tt{<p>}, @tt{div},
@|etc|).}


@defstruct[column-attributes ([assoc (listof (cons/c symbol? string?))])]{

Used as a @tech{style property} on a style with @racket[table-columns]
to add arbitrary attributes to an HTML @tt{col} tag within the table.}


@defstruct[url-anchor ([name string?])]{

Used as a @tech{style property} with @racket[element] to insert an
anchor before the element.}


@defstruct[hover-property ([text string?])]{

Used as a @tech{style property} with @racket[element] to add text that
is shown when the mouse hovers over the element.}


@defstruct[script-property ([type string?]
                           [script (or/c path-string? (listof string?))])]{

Used as a @tech{style property} with @racket[element] to supply a
script alternative to the element content.}


@defstruct[css-addition ([path (or/c path-string? 
                                     (cons/c 'collects (listof bytes?))
                                     url?
                                     bytes?)])]{

Used as a @tech{style property} to supply a CSS file (if @racket[path]
is a path, string, or list), URL (if @racket[path] is a @racket[url]) or content (if @racket[path] is a byte
string) to be referenced or included in the generated HTML. This
property can be attached to any style, and all additions are collected
to the top of the generated HTML page.

The @racket[path] field can be a result of
@racket[path->main-collects-relative].}

@defstruct[css-style-addition ([path (or/c path-string? 
                                           (cons/c 'collects (listof bytes?))
                                           url?
                                           bytes?)])]{

Like @racket[css-addition], but added after any style files that are
specified by a document and before any style files that are provided
externally.}


@defstruct[js-addition ([path (or/c path-string? 
                                    (cons/c 'collects (listof bytes?))
                                    url?
                                    bytes?)])]{

Like @racket[css-addition], but for a JavaScript file instead of a CSS file.}


@defstruct[js-style-addition ([path (or/c path-string? 
                                          (cons/c 'collects (listof bytes?))
                                          url?
                                          bytes?)])]{

Like @racket[css-style-addition], but for a JavaScript file instead of a CSS file.}


@defstruct[body-id ([value string?])]{

Used as a @tech{style property} to associate an @tt{id} attribute with
an HTML tag.}


@defstruct[document-source ([module-path module-path?])]{

Used as a @tech{style property} to associate a module path with a
part.  Clicking on a section title within the part may show
@racket[module-path] with the part's tag string, so that authors of
other documents can link to the section.

More specifically, the section title is given the HTML attributes
@tt{x-source-module} and @tt{x-part-tag}, plus @tt{x-part-prefixes}
if the section or enclosing sections declare tag prefixes, and
@tt{x-source-pkg} if the source is found within a package at document-build time. The
@racketmodname[scribble/manual] style recognizes those tags to make
clicking a title show cross-reference information.

@history[#:added "1.2"
         #:changed "1.7" @elem{Added @tt{x-part-prefixes}.}
         #:changed "1.9" @elem{Added @tt{x-source-pkg}.}]}


@defstruct[html-defaults ([prefix (or/c bytes? path-string? 
                                        (cons/c 'collects (listof bytes?)))]
                          [style (or/c bytes? path-string? 
                                       (cons/c 'collects (listof bytes?)))]
                          [extra-files (listof (or/c path-string? 
                                                     (cons/c 'collects (listof bytes?))))])]{

Like @racket[latex-defaults], but use for the 
@exec{scribble} command-line tool's @DFlag{html} and
@DFlag{htmls} modes.}


@defstruct[head-extra ([xexpr xexpr/c])]{

For a @racket[part] that corresponds to an HTML page, adds content to
the @tt{<head>} tag.}


@defstruct[render-convertible-as ([types (listof (or/c 'png-bytes 'svg-bytes))])]{
 For a @racket[part] that corresponds to an HTML page,
 controls how objects that subscribe to the @racketmodname[file/convertible]
 protocol are rendered.
      
 The alternatives in the @racket[types] field are tried in order
 and the first one that succeeds is used in the html output.
}

@defstruct[part-link-redirect ([url url?])]{

As a @tech{style property} on a @tech{part}, causes hyperiinks to the
part to be redirected to @racket[url] instead of the rendered part.}

@defstruct[link-resource ([path path-string?])]{

As a @tech{style property} on an @racket[element], causes the elements
to be rendered as a hyperlink to (a copy of) @racket[path].

The file indicated by @racket[path] is referenced in place when
@racket[render<%>] is instantiated with
@racketidfont{refer-to-existing-files} as true. Otherwise, it is
copied to the destination directory and potentially renamed to avoid
conflicts.}


@defstruct[install-resource ([path path-string?])]{

Like @racket[link-resource], but makes @racket[path] accessible in the
destination without rendering a hyperlink.

This @tech{style property} is useful only when @racket[render<%>] is
instantiated with @racketidfont{refer-to-existing-files} as
@racket[#f], and only when @racket[path] does not match then name of
any other file that is copied by the renderer to the destination.}

@; ----------------------------------------

@section{Latex Style Properties}

@defmodule[scribble/latex-properties]{ The
@racket[scribble/latex-properties] library provides datatypes used as
@tech{style properties} for Latex rendering.}


@defstruct[tex-addition ([path (or/c path-string? 
                                     (cons/c 'collects (listof bytes?))
                                     bytes?)])]{

Used as a @tech{style property} to supply a @filepath{.tex} file (if
@racket[path] is a path, string, or list) or content (if @racket[path]
is a byte string) to be included in the generated Latex. This property
can be attached to any style, and all additions are collected to the
top of the generated Latex file.

The @racket[path] field can be a result of
@racket[path->main-collects-relative].}


@defstruct[latex-defaults ([prefix (or/c bytes? path-string? 
                                         (cons/c 'collects (listof bytes?)))]
                           [style (or/c bytes? path-string? 
                                        (cons/c 'collects (listof bytes?)))]
                           [extra-files (listof (or/c path-string? 
                                                      (cons/c 'collects (listof bytes?))))])]{

Used as a @tech{style property} on the main @racket[part] of a document
to set a default prefix file, style file, and extra files (see
@secref["config-style"]).  The defaults are used by the
@exec{scribble} command-line tool for @DFlag{latex} or @DFlag{pdf}
mode if none are supplied via @DFlag{prefix} and @DFlag{style} (where
@racket[extra-files] are used only when @racket[prefix] is used). A
byte-string value is used directly like file content, and a path can
be a result of @racket[path->main-collects-relative].

Languages (used with @hash-lang[]) like
@racketmodname[scribble/manual] and @racketmodname[scribble/sigplan]
add this property to a document to specify appropriate files for Latex
rendering.

See also @racketmodname[scribble/latex-prefix].}

@defstruct[(latex-defaults+replacements latex-defaults)
           ([replacements (hash/c string? (or/c bytes? path-string?
                                                (cons/c 'collects (listof bytes?))))])]{
  Like @racket[latex-defaults] but it allows for more configuration. For example if
  the @racket[replacements] maps @racket["scribble-load-replace.tex"] to @racket["my-scribble.tex"],
  then the @racket["my-scribble.tex"] file in the current directory will we used in place
  of the standard scribble package inclusion header.
}


@defstruct[command-extras ([arguments (listof string?)])]{

Used as a @tech{style property} on an @racket[element] to add extra
arguments to the element's command in Latex output.}

@defstruct[command-optional ([arguments (listof string?)])]{
                                                  
 Used as a @tech{style property} on a @racket[element] to add
 a optional arguments to the element's command in Latex output.

 @history[#:added "1.20"]
}

@defstruct[short-title ([text (or/c string? #f)])]{
                                                  
 Used as a @tech{style property} on a @racket[title-decl].
 Attaches a short title to the title for a @racket[part] if
 the Latex class file uses a short title.

 @history[#:added "1.20"]
}

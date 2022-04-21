#lang scribble/manual
@(require "utils.rkt"
          scribble/bnf
          (for-label setup/xref
                     compiler/cm))

@(define fn (italic "fn"))

@title[#:tag "running"]{Running @exec{scribble}}

The @exec{scribble} command-line tool (also available as @as-index{@exec{raco
scribble}}) runs a Scribble document and renders it to a specific
format. Select a format with one of the following flags, where the
output name @|fn| is by default the document source name without
its file suffix:

@itemlist[

 @item{@DFlag{html} --- a single HTML page @filepath{@|fn|.html},
       plus CSS sources and needed image files; this mode is the
       default if no format is specified}

 @item{@DFlag{htmls} --- multiple HTML pages (and associated files) in
       a @filepath{@|fn|} directory, starting with
       @filepath{@|fn|/index.html}}

 @item{@DFlag{html-tree} @nonterm{n} --- HTML pages in a directory
       tree up to @nonterm{n} layers deep; a tree of depth @exec{0} is
       equivalent to using @DFlag{html}, and a tree of depth @exec{1}
       is equivalent to using @DFlag{htmls}}

 @item{@DFlag{latex} --- LaTeX source @filepath{@|fn|.tex}, plus
       any needed additional files (such as non-standard class files)
       needed to run @exec{latex} or @exec{pdflatex}}

 @item{@DFlag{pdf} --- PDF @filepath{@|fn|.pdf} that is generated
       via @exec{pdflatex}}

 @item{@DFlag{xelatex} --- PDF @filepath{@|fn|.pdf} that is generated
       via @exec{xelatex}}

 @item{@DFlag{lualatex} --- PDF @filepath{@|fn|.pdf} that is generated
       via @exec{lualatex}}

 @item{@DFlag{dvipdf} --- PDF @filepath{@|fn|.pdf} that is generated
       via @exec{latex}, @exec{dvips}, and @exec{pstopdf}}

 @item{@DFlag{latex-section} @nonterm{n} --- LaTeX source
       @filepath{@|fn|.tex} plus additional @filepath{.tex} files to
       be included in the enclosing document's preamble, where the
       enclosing document must use the UTF-8 input encoding and T1
       font encoding; use @tt{1} for @nonterm{n} to make the rendered
       document a section, @tt{2} for a subsection, etc.}

 @item{@DFlag{text} --- plain text in a single file
       @filepath{@|fn|.txt}, with non-ASCII content encoded as UTF-8}

 @item{@DFlag{markdown} --- Markdown text in a single file
       @filepath{@|fn|.md}, with non-ASCII content encoded as UTF-8}

]

Use @DFlag{dest-name} to specify a @|fn| other than the default name,
but only when a single source file is provided. Use the @DFlag{dest}
flag to specify a destination directory (for any number of source
files). Use @DFlag{dest-base} to add a prefix to the name of each
support file that is generated or copied to the destination; the prefix
can contain a directory path, and a non-directory ending element is
used as a prefix on a support-file name. Use @DFlag{keep-at-dest-base}
to avoid overwriting existing files with support files (but existing
files are used when the content matches what would be written otherwise).

After all flags, provide one or more document sources, where each
source declares a module. The module should either have a @racket[doc]
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{submodule}
that exports @racket[doc] as a @racket[part], or it should directly
export @racket[doc] as a @racket[part]. (The submodule is tried first,
and the main module is not directly loaded or evaluated if the
submodule can be loaded on its own.) Use @DFlag{doc-binding} to
access an alternate exported name in place of @racket[doc].

When multiple documents are rendered at the same time, cross-reference
information in one document is visible to the other documents. See
@secref["xref-flags"] for information on references that cross
documents that are built separately.

@history[#:changed "1.4" @elem{Added @DFlag{dvipdf}.}
         #:changed "1.18" @elem{Added @DFlag{doc-binding}.}
         #:changed "1.19" @elem{Added @DFlag{xelatex}.}
         #:changed "1.40" @elem{Added @DFlag{keep-at-dest-base} and
                                fixed @DFlag{dest-base} to work correctly
                                for HTML output.}
         #:changed "1.45" @elem{Added @DFlag{lualatex}.}]

@section{Extra and Format-Specific Files}

Use the @DFlag{style} flag to specify a format-specific file to adjust
the output style file for certain formats. For HTML (single-page or
multi-page) output, the style file should be a CSS file that is
applied after all other CSS files, and that may therefore override
some style properties. For Latex (or PDF) output, the style file
should be a @filepath{.tex} file that can redefine Latex commands.
When a particular Scribble function needs particular CSS or Latex
support, however, a better option is to use a @racket[css-addition] or
@racket[tex-addition] style property so that the support is included
automatically; see @secref["config"] for more information.

In rare cases, use the @DFlag{style} flag to specify a format-specific
base style file. For HTML (single-page or multi-page) output, the
style file should be a CSS file to substitute for
@filepath{scribble.css} in the @filepath{scribble} collection. For
Latex (or PDF) output, the style file should be a @filepath{.tex} file
to substitute for @filepath{scribble.tex} in the @filepath{scribble}
collection. The @DFlag{style} flag is rarely useful, because the
content of @filepath{scribble.css} or @filepath{scribble.tex} is
weakly specified; replacements must define all of the same styles, and
the set of styles can change across versions of Racket.

Use @DFlag{prefix} to specify an alternate format-specific start of
the output file. For HTML output, the starting file specifies the
@tt{DOCTYPE} declaration of each output HTML file as a substitute for
@filepath{scribble-prefix.html} in the @filepath{scribble}
collection. For Latex (or PDF) output (but not Latex-section output), the starting file specifies
the @ltx{documentclass} declaration and initial @ltx{usepackage}
declarations as a substitute for @filepath{scribble-prefix.tex} in the
@filepath{scribble} collection. See also @racket[html-defaults],
@racket[latex-defaults], and @secref["config"].

For any output form, use the @DPFlag{extra} flag to add a needed file
to the build destination, such as an image file that is referenced in
the generated output but not included via @racket[image] (which copies
the file automatically).

@section[#:tag "xref-flags"]{Handling Cross-References}

Cross references within a document or documents rendered together are
always resolved. When cross references span documents that are
rendered separately, cross-reference information needs to be saved and
loaded explicitly. Cross-reference information is format-specific, but
HTML-format information is usable for Latex (or PDF) or text rendering.

A Racket installation includes HTML-format cross-reference information
for all installed documentation. Each document's information is in a
separate file, so that loading all relevant files would be tedious.
The @PFlag{m} or @DPFlag{main-xref-in} flag loads cross-reference
information for all installed documentation, so

@commandline{scribble +m mine.scrbl}

renders @filepath{mine.scrbl} to @filepath{mine.html} with
cross-reference links to the Racket installation's documentation.
(The @filepath{racket-index} package must be installed to use
@PFlag{m}/@DPFlag{main-xref-in}.)

The @DPFlag{xref-in} flag loads cross-reference information by calling
a specified module's function. The @racketmodname[setup/xref] module
provides @racket[load-collections-xref] to load cross-reference
information for all installed documentation, and @PFlag{m} or
@DPFlag{main-xref-in} is just a shorthand for @exec{++xref-in
setup/xref load-collections-xref}.

The @DFlag{redirect-main} flag for HTML output redirects links to the local
installation's documentation (not user-scope documentation) to a given URL, such as
@tt{http://docs.racket-lang.org/}. Beware that documentation links
sometimes change (although Scribble generates HTML paths and anchors
in a relatively stable way), so
@tt{http://download.racket-lang.org/docs/@italic{version}/html/} may be
more reliable when building with an installation for @italic{version}.
The @DFlag{redirect-main} flag is ignored for non-HTML output.

The @DFlag{redirect} flag is like @DFlag{redirect-main}, except that
it builds on the given URL to indicate a cross-reference tag that is
more stable than an HTML path and anchor (in case the documentation
for a function changes sections, for example), and it can generate
redirected linked for documentation that is installed in user scope.
The URL @tt{https://docs.racket-lang.org/local-redirect/index.html} can
work for these redirections.

For cross-references among documentation that is not part of the
Racket installation, use @DFlag{info-out} to save information from a
document build and use @DPFlag{info-in} to load previously saved
information. For example, if @filepath{c.scrbl} refers to information
in @filepath{a.scrbl} and @filepath{b.scrbl}, then

@commandline{scribble --info-out a.sxref a.scrbl}
@commandline{scribble --info-out b.sxref b.scrbl}
@commandline{scribble ++info-in a.sxref ++info-in b.sxref c.scrbl}

builds @filepath{c.html} with cross-reference links into
@filepath{a.html} and @filepath{b.html}.


@section{Selecting an Image Format}

Use the @DPFlag{convert} @nonterm{fmt} flag to select @nonterm{fmt} as
a preferred image format to use when rendering a document that
includes values that can be converted to different image formats. The
@nonterm{fmt} argument can be @exec{pdf}, @exec{ps}, @exec{png},
@exec{svg}, or @exec{gif}, but a renderer typically supports only a
subset of those formats.

Use @DPFlag{convert} @nonterm{fmt} multiple times to specify multiple
preferred formats, where a @nonterm{fmt} earlier in the command line
take precedence over @nonterm{fmt}s specified later.

For example, to generate Latex sources with images in Encapsulated
PostScript format (so that the result works with @exec{latex} instead
of @exec{pdflatex}), combine @DFlag{latex} with @exec{@DPFlag{convert}
ps}. To generate HTML pages with images converted to SVG format
instead of PNG format, combine @DFlag{html} with
@exec{@DPFlag{convert} svg}.

@history[#:changed "1.4" @elem{Added @DPFlag{convert} support.}]

@section{Passing Command-Line Arguments to Documents}

When @exec{scribble} loads and renders a document module, by default
it sets @racket[current-command-line-arguments] to an empty vector.
Use the @DPFlag{arg} flag (any number of times) to add a string to
@racket[current-command-line-arguments].

For example,

@commandline{scribble ++arg --mode ++arg fast turtle.scrbl}

causes @racket[(current-command-line-arguments)] to return
@racket['#("--mode" "fast")] while @filepath{turtle.scrbl} is loaded
and rendered, which could affect the content that
@filepath{turtle.scrbl} generates if it uses
@racket[current-command-line-arguments].

@history[#:changed "1.1" @elem{Added the empty-vector default and @DPFlag{arg} flag.}]

@section{Additional Options}

@itemlist[
 @item{@DFlag{quiet} --- suppress output-file and undefined-tag reporting}

 @item{@DFlag{make} or @Flag{y} --- Enable automatic generation and
       update of compiled @filepath{.zo} files when loading document
       modules. Specifically, the result of
       @racket[(make-compilation-manager-load/use-compiled-handler)]
       is installed as the value of @racket[current-load/use-compiled]
       while loading a module.}

 @item{@DFlag{doc-binding} @nonterm{id} --- render document
       provided as @nonterm{id} instead of @racket[doc]}

 @item{@DFlag{errortrace} --- enable @racketmodname[errortrace #:indirect]}
]

@history[#:changed "1.38" @elem{Added the @DFlag{errortrace} flag.}
         #:changed "1.44" @elem{Added the @DFlag{make}/@Flag{y} flag.}]

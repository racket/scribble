#lang scribble/doc
@(require scribble/manual 
          "utils.rkt" 
          (for-label racket/class
                     scribble/render
                     scribble/xref
                     setup/dirs))

@(define-syntax-rule (defmodule/local lib . content)
   (begin
     (define-syntax-rule (intro)
       (begin
         (require (for-label lib))
         (defmodule lib)
         . content))
     (intro)))

@(begin
  (define-syntax-rule (def-html-render-mixin id mid)
    (begin
      (require (for-label scribble/html-render))
      (define id @racket[render-mixin])
      (define mid @racket[render-multi-mixin])))
  (def-html-render-mixin html:render-mixin html:render-multi-mixin))
@(begin
  (define-syntax-rule (def-latex-render-mixin id)
    (begin
      (require (for-label scribble/latex-render))
      (define id @racket[render-mixin])))
  (def-latex-render-mixin latex:render-mixin))

@title[#:tag "renderer"]{Renderers}

A renderer is an object that provides four main methods:
@racket[traverse], @racket[collect], @racket[resolve], and
@racketidfont{render}. Each method corresponds to a pass described in
@secref["core"], and they are chained together by the @racket[render]
function to render a document.

@section{Rendering Driver}

@defmodule[scribble/render]

@defproc[(render [docs (listof part?)]
                 [names (listof path-string?)]
                 [#:render-mixin render-mixin (class? . -> . class?) @#,html:render-mixin]
                 [#:dest-dir dest-dir (or/c #f path-string?) #f]
                 [#:helper-file-prefix helper-file-prefix (or/c #f string?) #f]
                 [#:keep-existing-helper-files? keep-existing-helper-files? any/c #f]
                 [#:prefix-file prefix-file (or/c #f path-string?) #f]
                 [#:style-file style-file (or/c #f path-string?) #f]
                 [#:style-extra-files style-extra-files (listof path-string?) #f]
                 [#:extra-files extra-files (listof path-string?) #f]
                 [#:image-preferences image-preferences (listof (or/c 'ps 'pdf 'png 'svg 'gif)) null]
                 [#:xrefs xrefs (listof xref?) null]
                 [#:info-in-files info-in-files (listof path-string?) null]
                 [#:info-out-file info-out-file (or/c #f path-string?) #f]
                 [#:redirect redirect (or/c #f string?) #f]
                 [#:redirect-main redirect-main (or/c #f string?) #f]
                 [#:directory-depth directory-depth exact-nonnegative-integer? 0]
                 [#:quiet? quiet? any/c #t]
                 [#:warn-undefined? warn-undefined? any/c (not quiet?)])
          void?]{

Renders the given @racket[docs], each with an output name derived from
the corresponding element of @racket[names]. A directory path (if any)
for a name in @racket[names] is discarded, and the file suffix is
replaced (if any) with a suitable suffix for the output format.

The @racket[render-mixin] argument determines the output format. By
default, it is @html:render-mixin from @racketmodname[scribble/html-render].

The @racket[dest-dir] argument determines the output directory, which
is created using @racket[make-directory*] if it is non-@racket[#f] and
does not exist already.

The @racket[helper-file-prefix], @racket[keep-existing-helper-files?],
@racket[prefix-file], @racket[style-file], @racket[style-extra-files],
and @racket[extra-files] arguments are passed on to the
@racket[render%] constructor.

The @racket[image-preferences] argument specified preferred formats
for image files and conversion, where formats listed earlier in the
list are more preferred. The renderer specified by
@racket[render-mixin] may not support all of the formats listed in
@racket[image-preferences].

The @racket[xrefs] argument provides extra cross-reference information
to be used during the documents' @tech{resolve pass}. The
@racket[info-in-files] arguments supply additional cross-reference
information in serialized form. When the @racket[info-out-file]
argument is not @racket[#f], cross-reference information for the
rendered documents is written in serialized for to the specified file.

The @racket[redirect] and @racket[redirect-main] arguments correspond
to the @racket[set-external-tag-path] and
@racket[set-external-root-url] methods of @|html:render-mixin| from
@racketmodname[scribble/html-render], so they should be
non-@racket[#f] only for HTML rendering.

The @racket[directory-depth] arguments correspond to the
@racket[set-directory-depth] method of @|html:render-multi-mixin|.

If @racket[quiet?] is a false value, output-file information is
written to the current output port.

If @racket[warn-undefined?] is a true value, then references to
missing cross-reference targets trigger a warning message on the
current error port.

@history[#:changed "1.4" @elem{Added the @racket[#:image-preferences] argument.}
         #:changed "1.40" @elem{Added the @DFlag{keep-existing-helper-files?}
                                initialization argument and
                                fixed @DFlag{helper-file-prefix} to work correctly
                                for HTML output.}]}


@section{Base Renderer}

@defmodule[scribble/base-render]{The
@racketmodname[scribble/base-render] module provides @racket[render%],
which implements the core of a renderer. This rendering class must be
refined with a mixin from @racketmodname[scribble/text-render],
@racketmodname[scribble/markdown-render], or
@racketmodname[scribble/html-render], or
@racketmodname[scribble/latex-render].}

The mixin structure is meant to support document-specific extensions
to the renderers. For example, the @exec{scribble} command-line tool
might, in the future, extract rendering mixins from a document module
(in addition to the document proper).

See the @filepath{base-render.rkt} source for more information about
the methods of the renderer. Documents built with higher layers, such
as @racketmodname[scribble/manual], generally do not call the render
object's methods directly.

@definterface[render<%> ()]{

@defmethod[(traverse [srcs (listof part?)]
                     [dests (listof path-string?)])
           (and/c hash? immutable?)]{

Performs the @techlink{traverse pass}, producing a hash table that
contains the replacements for and @racket[traverse-block]s and
@racket[traverse-elements]s. See @method[render<%> render] for
information on the @racket[dests] argument.}

@defmethod[(collect [srcs (listof part?)]
                    [dests (listof path-string?)]
                    [fp (and/c hash? immutable?)]
                    [demand (tag? collect-info? . -> . any/c) (lambda (_tag _ci) #f)])
           collect-info?]{

Performs the @techlink{collect pass}. See @method[render<%> render] for
information on the @racket[dests] arguments. The @racket[fp] argument
is a result from the @method[render<%> traverse] method.

The @racket[demand] argument supplies external tag mappings on demand.
When the @racket[collect-info] result is later used to find a mapping
for a tag and no mapping is already available, @racket[demand] is
called with the tag and the @racket[collect-info]. The @racket[demand]
function returns true to indicate when it adds information to the
@racket[collect-info] so that the lookup should be tried again; the
@racket[demand] function should return @racket[#f] if it does not
extend @racket[collect-info].}

@defmethod[(resolve [srcs (listof part?)]
                    [dests (listof path-string?)]
                    [ci collect-info?])
           resolve-info?]{

Performs the @techlink{resolve pass}. See @method[render<%> render] for
information on the @racket[dests] argument.  The @racket[ci] argument
is a result from the @method[render<%> collect] method.}

@defmethod[(render [srcs (listof part?)]
                   [dests (listof (or/c path-string? #f))]
                   [ri resolve-info?])
           list?]{

Produces the final output.  The @racket[ri] argument is a result from
the @method[render<%> render] method.

The @racket[dests] provide names of files for Latex or single-file
HTML output, or names of sub-directories for multi-file HTML output.
If the @racket[dests] are relative, they're relative to the current
directory; normally, they should indicates a path within the
@racket[_dest-dir] supplied on initialization of the @racket[render%]
object.

If an element of @racket[dests] is @racket[#f], then the corresponding
position of the result list contains a string for rendered document.
Some renderers require that @racket[dest] contains all path strings.}


@defmethod[(serialize-info [ri resolve-info?])
           any/c]{

Serializes the collected info in @racket[ri].}


@defmethod[(serialize-infos [ri resolve-info?] 
                            [count exact-positive-integer?] 
                            [doc part?])
           list?]{

Like @method[render<%> serialize-info], but produces @racket[count] results
that together have the same information as produced by
@method[render<%> serialize-info]. The structure of @racket[doc] is used to
drive the partitioning (on the assumption that @racket[ri] is derived
from @racket[doc]).}


@defmethod[(deserialize-info [v any/c]
                             [ci collect-info?]
                             [#:root root-path (or/c path-string? false/c) #f])
           void?]{

Adds the deserialized form of @racket[v] to @racket[ci].

If @racket[root-path] is not @racket[#f], then file paths that are
recorded in @racket[ci] as relative to an instantiation-supplied
@racket[root-path] are deserialized as relative instead to the given
@racket[root-path].}


@defmethod[(get-defined [ci collect-info?]) (listof tag?)]{

Returns a list of tags that were defined within the documents
represented by @racket[ci].}


@defmethod[(get-defineds [ci collect-info?] 
                         [count exact-positive-integer?] 
                         [doc part?])
           (listof (listof tag?))]{

Analogous to @method[render<%> serialize-infos]: returns a list of
tags for each of @racket[count] partitions of the result of
@method[render<%> get-defined], using the structure of @racket[doc] to
drive the partitioning.}


@defmethod[(get-external [ri resolve-info?]) (listof tag?)]{

Returns a list of tags that were referenced but not defined within the
documents represented by @racket[ri] (though possibly found in
cross-reference information transferred to @racket[ri] via
@racket[xref-transfer-info]).}


@defmethod[(get-undefined [ri resolve-info?]) (listof tag?)]{

Returns a list of tags that were referenced by the resolved documents
with no target found either in the resolved documents represented by
@racket[ri] or cross-reference information transferred to @racket[ri]
via @racket[xref-transfer-info].

If multiple tags were referenced via @racket[resolve-search] and a
target was found for any of the tags using the same dependency key,
then no tag in the set is included in the list of undefined tags.}

}

@defclass[render% object% (render<%>)]{

Represents a renderer.

@defconstructor[([dest-dir path-string?]
                 [refer-to-existing-files any/c #f]
                 [root-path (or/c path-string? #f) #f]
                 [prefix-file (or/c path-string? #f) #f]
                 [style-file (or/c path-string? #f) #f]
                 [style-extra-files (listof path-string?) null]
                 [extra-files (listof path-string?) null]
                 [helper-file-prefix (or/c path-string? #f) #f]
                 [keep-existing-helper-files? any/c #f]
                 [image-preferences (listof (or/c 'ps 'pdf 'png 'svg 'gif)) null])]{

Creates a renderer whose output will go to @racket[dest-dir]. For
example, @racket[dest-dir] could name the directory containing the
output Latex file, the HTML file for a single-file output, or the
output sub-directory for multi-file HTML output.

If @racket[refer-to-existing-files] is true, then when a document
refers to external files, such as an image or a style file, then the
file is referenced from its source location instead of copied to the
document destination.

If @racket[root-path] is not @racket[#f], it is normally the same as
@racket[dest-dir] or a parent of @racket[dest-dir]. It causes
cross-reference information to record destination files relative to
@racket[root-path]; when cross-reference information is serialized, it
can be deserialized via @method[render<%> deserialize-info] with a
different root path (indicating that the destination files have
moved).

The @racket[prefix-file], @racket[style-file], and
@racket[style-extra-files] arguments set files that control output
styles in a formal-specific way; see @secref["config-style"] for more
information.

The @racket[extra-files] argument names files to be copied to the
output location, such as image files or extra configuration files.

The @racket[helper-file-prefix] argument supplies a prefix that is
used for any copied or generated files used by the main destination
file. This prefix is not used for files listed in
@racket[extra-files]. If @racket[keep-existing-helper-files?] is true,
then any existing file that would otherwise be overwritten with a
helper file is instead preserved, and the helper file is written to a
different name, unless its content would be exactly the same as the
existing file.

The @racket[image-preferences] argument specified preferred formats
for image files and conversion, where formats listed earlier in the
list are more preferred. The renderer may not support all of the
formats listed in @racket[image-preferences].

@history[#:changed "1.4" @elem{Added the @racket[image-preferences]
                               initialization argument.}
         #:changed "1.40" @elem{Added the @DFlag{keep-existing-helper-files?}
                                initialization argument and
                                fixed @DFlag{helper-file-prefix} to work correctly
                                for HTML output.}]}

@defmethod[(traverse [parts (listof part?)]
                     [dests (listof path-string?)])
           (and/c hash? immutable?)]
@defmethod[(start-traverse [parts (listof part?)]
                           [dests (listof path-string?)]
                           [fp (and/c hash? immutable?)])
           (and/c hash? immutable?)]
@defmethod[(traverse-part [p part?]
                          [fp (and/c hash? immutable?)])
            (and/c hash? immutable?)]
@defmethod[(traverse-flow [bs (listof block?)]
                          [fp (and/c hash? immutable?)])
           (and/c hash? immutable?)]
@defmethod[(traverse-block [b block?]
                           [fp (and/c hash? immutable?)])
           (and/c hash? immutable?)]
@defmethod[(traverse-nested-flow [nf nested-flow?]
                                 [fp (and/c hash? immutable?)])
           (and/c hash? immutable?)]
@defmethod[(traverse-table [t table?]
                           [fp (and/c hash? immutable?)])
           (and/c hash? immutable?)]
@defmethod[(traverse-itemization [i itemization?] 
                                 [fp (and/c hash? immutable?)])
           (and/c hash? immutable?)]
@defmethod[(traverse-compound-paragraph [cp compound-paragraph?]
                                        [fp (and/c hash? immutable?)])
           (and/c hash? immutable?)]
@defmethod[(traverse-paragraph [p paragraph?]
                               [fp (and/c hash? immutable?)])
           (and/c hash? immutable?)]
@defmethod[(traverse-content [c content?]
                             [fp (and/c hash? immutable?)])
           (and/c hash? immutable?)]
@defmethod[(traverse-target-element [e target-element?]
                                    [fp (and/c hash? immutable?)])
           (and/c hash? immutable?)]
@defmethod[(traverse-index-element [e index-element?]
                                   [fp (and/c hash? immutable?)])
           (and/c hash? immutable?)]{

These methods implement the @tech{traverse pass} of document rendering.
Except for the entry point @method[render% traverse] as described by
as described at @xmethod[render<%> traverse], these methods
generally would not be called to render a document, but instead
provide natural points to interpose on the default implementation.

A renderer for a specific format is relatively unlikely to override
any of these methods. Each method accepts the information accumulated
so far and returns augmented information as a result.}


@defmethod[(collect [parts (listof part?)]
                    [dests (listof path-string?)]
                    [fp (and/c hash? immutable?)]
                    [demand (tag? collect-info? . -> . any/c) (lambda (_tag _ci) #f)])
           collect-info?]
@defmethod[(start-collect [parts (listof part?)]
                          [dests (listof path-string?)]
                          [ci collect-info?])
           void?]
@defmethod[(collect-part [p part?]
                         [parent (or/c #f part?)]
                         [ci collect-info?]
                         [number (listof part-number-item?)]
                         [init-sub-number part-number-item?]
                         [init-sub-numberers (listof numberer?)])
            (values part-number-item? numberer?)]
@defmethod[(collect-part-tags [p part?]
                              [ci collect-info?]
                              [number (listof part-number-item?)])
           void?]
@defmethod[(collect-flow [bs (listof block?)]
                         [ci collect-info?])
           void?]
@defmethod[(collect-block [b block?]
                          [ci collect-info?])
           void?]
@defmethod[(collect-nested-flow [nf nested-flow?]
                                [ci collect-info?])
           void?]
@defmethod[(collect-table [t table?]
                          [ci collect-info?])
           void?]
@defmethod[(collect-itemization [i itemization?]
                                [ci collect-info?])
           void?]
@defmethod[(collect-compound-paragraph [cp compound-paragraph?]
                                       [ci collect-info?])
           void?]
@defmethod[(collect-paragraph [p paragraph?]
                              [ci collect-info?])
           void?]
@defmethod[(collect-content [c content?]
                            [ci collect-info?])
           void?]
@defmethod[(collect-target-element [e target-element?]
                                   [ci collect-info?])
           void?]
@defmethod[(collect-index-element [e index-element?]
                                  [ci collect-info?])
           void?]{

These methods implement the @tech{collect pass} of document rendering.
Except for the entry point @method[render% collect] as described at
@xmethod[render<%> collect], these methods generally would not be
called to render a document, but instead provide natural points to
interpose on the default implementation.

A renderer for a specific format is most likely to override
@method[render% collect-part-tags], @method[render%
collect-target-element], and perhaps @method[render% start-collect] to
set up and record cross-reference information in a way that is
suitable for the target format.}

@defmethod[(resolve [parts (listof part?)]
                    [dests (listof path-string?)]
                    [ci collect-info?])
           resolve-info?]
@defmethod[(start-resolve [parts (listof part?)]
                          [dests (listof path-string?)]
                          [ri resolve-info?])
           void?]
@defmethod[(resolve-part [p part?]
                         [ri resolve-info?])
            void?]
@defmethod[(resolve-flow [bs (listof block?)]
                         [enclosing-p part?]
                         [ri resolve-info?])
           void?]
@defmethod[(resolve-block [b block?]
                         [enclosing-p part?]
                          [ri resolve-info?])
           void?]
@defmethod[(resolve-nested-flow [nf nested-flow?]
                                [enclosing-p part?]
                                [ri resolve-info?])
           void?]
@defmethod[(resolve-table [t table?]
                          [enclosing-p part?]
                          [ri resolve-info?])
           void?]
@defmethod[(resolve-itemization [i itemization?]
                                [enclosing-p part?]
                                [ri resolve-info?])
           void?]
@defmethod[(resolve-compound-paragraph [cp compound-paragraph?]
                                       [enclosing-p part?]
                                       [ri resolve-info?])
           void?]
@defmethod[(resolve-paragraph [p paragraph?]
                              [enclosing-p part?]
                              [ri resolve-info?])
           void?]
@defmethod[(resolve-content [c content?]
                            [enclosing-p part?]
                            [ri resolve-info?])
           void?]{

These methods implement the @tech{resolve pass} of document rendering.
Except for the entry point @method[render% resolve] as described at
@xmethod[render<%> resolve], these methods generally would not be
called to render a document, but instead provide natural points to
interpose on the default implementation.

A renderer for a specific format is unlikely to override any of these
methods. Each method for a document fragment within a part receives
the enclosing part as an argument, as well as resolve information as
@racket[ri] to update.}


@defmethod[(render [parts (listof part?)]
                   [dests (listof (or/c path-string? #f))]
                   [ri resolve-info?])
           list?]
@defmethod[(render-one [part part?]
                       [ri resolve-info?]
                       [dest (or/c path-string? #f)])
           any/c]
@defmethod[(render-part [p part?]
                        [ri resolve-info?])
           any/c]
@defmethod[(render-part-content [p part?]
                                [ri resolve-info?])
           any/c]
@defmethod[(render-flow [bs (listof block?)]
                        [enclosing-p part?]
                        [ri resolve-info?]
                        [first-in-part-or-item? boolean?])
           any/c]
@defmethod[(render-block [b block?]
                         [enclosing-p part?]
                         [ri resolve-info?]
                         [first-in-part-or-item? boolean?])
           any/c]
@defmethod[(render-nested-flow [nf nested-flow?]
                               [enclosing-p part?]
                               [ri resolve-info?]
                               [first-in-part-or-item? boolean?])
           any/c]
@defmethod[(render-table [t table?]
                         [enclosing-p part?]
                         [ri resolve-info?]
                         [first-in-part-or-item? boolean?])
           any/c]
@defmethod[(render-auxiliary-table [t table?]
                                   [enclosing-p part?]
                                   [ri resolve-info?])
           any/c]
@defmethod[(render-itemization [i itemization?]
                               [enclosing-p part?]
                               [ri resolve-info?])
           any/c]
@defmethod[(render-compound-paragraph [cp compound-paragraph?]
                                      [enclosing-p part?]
                                      [ri resolve-info?]
                                      [first-in-part-or-item? boolean?])
           any/c]
@defmethod[(render-intrapara-block [p paragraph?]
                                   [enclosing-p part?]
                                   [ri resolve-info?]
                                   [first-in-compound-paragraph? boolean?]
                                   [last-in-compound-paragraph? boolean?]
                                   [first-in-part-or-item? boolean?])
           any/c]
@defmethod[(render-paragraph [p paragraph?]
                             [enclosing-p part?]
                             [ri resolve-info?])
           any/c]
@defmethod[(render-content [c content?]
                           [enclosing-p part?]
                           [ri resolve-info?])
           any/c]
@defmethod[(render-other [c (and/c content? (not/c element?) (not/c convertible?))]
                         [enclosing-p part?]
                         [ri resolve-info?])
           any/c]{

These methods implement the @tech{render pass} of document rendering.
Except for the entry point @method[render% render] as described at
@xmethod[render<%> render], these methods generally would not be
called to render a document, but instead provide natural points to
interpose on the default implementation.

A renderer for a specific format is likely to override most or all of
these methods. The result of each method can be anything, and the
default implementations of the methods propagate results and collect
them into a list as needed. The value of @racket[current-output-port]
is set by @method[render% render] for each immediate @racket[part]
before calling @method[render% render-one], so methods might
individually print to render, or they might return values that are
used both other methods to print. The interposition points for this
pass are somewhat different than for other passes:

@itemlist[

 @item{@method[render% render-one] is called by the @method[render%
       render] method on each immediate @racket[part] in the list for
       its first argument.}

 @item{@method[render% render-auxiliary-table] is called by the
       default @method[render% render-block] on any @racket[table]
       that has the @racket['aux] @tech{style property}.}

 @item{@method[render% render-intrapara-block] is called on blocks
       within a @racket[compound-paragraph], where the default
       implementation just chains to @racket[render% render-block].}


 @item{@method[render% render-other] is called by the default
       implementation of @racket[render-content] for any content that
       does not satisfy @racket[element?] or @racket[convertible?].}

]}

}

@; ----------------------------------------

@section{Text Renderer}

@defmodule/local[scribble/text-render]{

@defmixin[render-mixin (render<%>) ()]{

Specializes a @racket[render<%>] class for generating plain text.}}

@; ----------------------------------------

@section{Markdown Renderer}

@defmodule/local[scribble/markdown-render]{

@defmixin[render-mixin (render<%>) ()]{

Specializes a @racket[render<%>] class for generating Markdown text.

Code blocks are marked using the
@hyperlink["http://github.github.com/github-flavored-markdown/"
"Github convention"] @verbatim{```racket} so that they are lexed and
formatted as Racket code.}}

@defboolparam[current-markdown-link-sections enabled?]{

Determines whether section links within an output document are
rendered as a section link. The default is @racket[#f].

@history[#:added "1.31"]}

@; ----------------------------------------

@section{HTML Renderer}

@defmodule/local[scribble/html-render]{

@defmixin[render-mixin (render<%>) ()]{

  @defconstructor/auto-super[([search-box? boolean? #f])]{
   Specializes a @racket[render<%>] class for generating
   HTML output. The arguments are the same as 
   @racket[render<%>], except for the addition of 
   @racket[search-box].

   If @racket[search-box?] is @racket[#t] and the document
   is created with @racket[scribble/manual], then it will be
   rendered with a search box, similar to this page. Note
   that the @racket[search-box?] argument does not create
   the search page itself. Rather, it passes the search
   query to whatever page is located at
   @tt{search/index.html}. The query is passed as an HTTP
   query string in the @tt{q} field.}

@defmethod[(set-external-tag-path [url string?]) void?]{

Configures the renderer to redirect links to external documents via
@racket[url], adding a @tt{tag} query element to the end of the
URL that contains the Base64-encoded, @racket[print]ed, serialized
original tag (in the sense of @racket[link-element]) for the link.
The result of @racket[get-doc-search-url] is intended for use as @racket[url].

If the link is based on a cross-reference entry that has a
document-identifying string (see @racket[load-xref] and its
@racket[#:doc-id] argument), the document identifier is added as a
@tt{doc} query element, and a path to the target within the
document is added as a @tt{rel} query element.}

@defmethod[(set-external-root-url [url string?]) void?]{

Configures the renderer to redirect links to documents installed in
the distribution's documentation directory to the given URL, using the
URL as a replacement to the path of the distribution's document
directory.}

}

@defmixin[render-multi-mixin (render<%>) ()]{

Further specializes a rendering class produced by
@racket[render-mixin] for generating multiple HTML
files.

@defmethod[(set-directory-depth [depth exact-nonnegative-integer?]) void?]{

Sets the depth of directory structure used when rendering parts that
are own their own pages. A value of @racket[0] is treated the same as
@racket[1].}

}

}

@; ----------------------------------------

@section{Latex Renderer}

@defmodule/local[scribble/latex-render]{

@defmixin[render-mixin (render<%>) ()]{

Specializes a @racket[render<%>] class for generating Latex input.}}

@defparam[extra-character-conversions convs (-> char? (or/c string? #f))]{
Function that maps (special) characters to strings corresponding to the Latex
code that should be used to render them. This function should return false for
any character it does not know how to handle.

Scribble already converts many special characters to the proper Latex
commands. This parameter should be used in case you need characters it does not
support yet.
}

@; ----------------------------------------

@section{PDF Renderer}

@defmodule/local[scribble/pdf-render]{

@defmixin[render-mixin (render<%>) ()]{

Specializes a @racket[render<%>] class for generating PDF output via
Latex, building on @|latex:render-mixin| from @racketmodname[scribble/latex-render].}

@defmixin[dvi-render-mixin (render<%>) ()]{

Like @racket[render-mixin], but generates PDF output via @exec{latex},
@exec{dvips}, and @exec{pstopdf}.

@history[#:added "1.4"]}}

@defmixin[xelatex-render-mixin (render<%>) ()]{

Like @racket[render-mixin], but generates PDF output via @exec{xelatex}.

@history[#:added "1.19"]}

@defmixin[lualatex-render-mixin (render<%>) ()]{

Like @racket[render-mixin], but generates PDF output via @exec{lualatex}.

@history[#:added "1.45"]}

@; ----------------------------------------

@section{Contract (Blue boxes) Renderer}

@defmodule/local[scribble/contract-render]{

@defmixin[override-render-mixin-multi (render<%>) ()]{

Overrides the @method[render<%> render] method of 
given renderer to record the content of the 
blue boxes (generated by @racket[defproc], @racket[defform], etc)
that appear in the document. 
   
@defmethod[#:mode override
                  (render [srcs (listof part?)]
                          [dests (listof path?)]
                          [ri render-info?])
                  void?]{
In addition to doing whatever the @racket[super] method
does, also save the content of the blue boxes (rendered
via a @racketmodname[scribble/text-render] renderer).
   
It saves this information in three pieces in a file
inside the @racket[dests] directories called
@filepath{blueboxes.rktd}. The first piece is
a single line containing a (decimal, ASCII) number. That number
is the number of bytes that the second piece of information
occupies in the file. The second piece of information
is a @racket[hash] that maps @racket[tag?] values to
a list of offsets and line numbers that follow the hash table.
For example, if the @racket[hash] maps
@racket['(def ((lib "x/main.rkt") abcdef))] to
@racket['((10 . 3))], then that means that the documentation
for the @racket[abcdef] export from the @racket[x] collection
starts 10 bytes after the end of the hash table and continues for
@racket[3] lines. Multiple elements in the list mean that that
@racket[tag?] has multiple blue boxes and each shows where one
of the boxes appears in the file.
}}
 
@defmixin[override-render-mixin-single (render<%>) ()]{

Just like @racket[override-render-mixin-multi], except
it saves the resulting files in a different place.

@defmethod[#:mode override
                  (render [srcs (listof part?)]
                          [dests (listof path?)]
                          [ri render-info?])
                  void?]{
  Just like @method[override-render-mixin-multi render], except
  that it saves the file @filepath{blueboxes.rktd} in
  the same directory where each @racket[dests] element resides.
}}
}

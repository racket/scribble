#lang scribble/manual
@(require (for-label scribble/core
                     scribble/decode
                     scriblib/figure
                     scheme/base
                     scheme/contract))

@(define-syntax-rule (sn s) @racket[s])

@title[#:tag "figure"]{Figures}

@defmodule[scriblib/figure]

@deftogether[(
@defproc[(figure [tag string?] [caption content?] 
                 [p pre-flow?] ...
                 [#:style style style? center-figure-style]
                 [#:label-sep label-sep pre-content? ": "]
                 [#:label-style label-style element-style? #f]
                 [#:continue? continue? any/c #f]) 
         block?]
@defproc[(figure* [tag string?] [caption content?]
                  [p pre-flow?] ...
                  [#:style style style? center-figure-style]
                  [#:label-sep label-sep pre-content? ": "]
                  [#:label-style label-style element-style? #f]
                  [#:continue? continue? any/c #f])
         block?]
@defproc[(figure** [tag string?] [caption content?]
                   [p pre-flow?] ...
                   [#:style style style? center-figure-style]
                   [#:label-sep label-sep pre-content? ": "]
                   [#:label-style label-style element-style? #f]
                   [#:continue? continue? any/c #f])
         block?]
@defproc[(figure-here [tag string?] [caption content?]
                      [pre-flow pre-flow?] ...
                      [#:style style style? center-figure-style]
                      [#:label-sep label-sep pre-content? ": "]
                      [#:label-style label-style element-style? #f]
                      [#:continue? continue? any/c #f])
         block?]
)]{

Creates a figure. The given @racket[tag] is for use with
@racket[figure-ref] or @racket[Figure-ref]. The @racket[caption] is an
element. The @racket[pre-flow] is decoded as a flow.

For HTML output, the @racket[figure] and @racket[figure*] functions
are the same, while @racket[figure**] allows the content to be wider
than the document body.  For two-column Latex output, @racket[figure*]
and @racket[figure**] generate a figure that spans columns.

For Latex output, @racket[figure-here] generates a figure to be included at
the position in the output text where the @racket[figure-here] occurs
in the source text. For HTML output, all @racket[figure] variants
place the figure where the use appears in the source text.

By default, @racket[style] is set so that the content of the figure is
centered.  Use @racket[left-figure-style], @racket[center-figure-style],
or @racket[right-figure-style] to specify the alignment.

The @racket[label-sep] and @racket[label-style] arguments adjust the
way that the caption's label is shown. By default, the label is the
word ``Figure'' followed by a space, the figure number, ``:'', and a
space, but @racket[label-sep] can specify an alternative to the ``:''
and ending space. The composed label is given the style specified by
@racket[label-style].

If @racket[continue?] is a true value, then the figure counter is not
incremented.

@history[#:changed "1.24" @elem{Added the @racket[#:label-sep] and
                                @racket[#:label-style] arguments.}]}

@deftogether[(
@defthing[left-figure-style style?]
@defthing[center-figure-style style?]
@defthing[right-figure-style style?]
@defthing[left style?]
)]{
Implements figure alignments.

The @racket[left] binding is a synonym for @racket[left-figure-style],
provided for backward compatibility.}


@defproc[(figure-ref [tag string?] ...+
                     [#:link-render-style link-style (or/c link-render-style? #f)])
         element?]{

Generates a reference to one or more figures, using a lowercase word ``figure''.

If @racket[link-style] or @racket[(current-link-render-style)] at the
time of rendering indicates the @racket['number] style mode, then the
word ``figure'' itself is not hyperlinked. Otherwise, the word
@racket[figure] is hyperlinked together with the referenced figure's
number.

@history[#:changed "1.26" @elem{Added the @racket[#:link-render-style] argument.}]}


@defproc[(Figure-ref [tag string?] ...+
                     [#:link-render-style link-style (or/c link-render-style? #f)])
         element?]{

Like @racket[figure-ref], but capitalizes the word ``Figure''.

@history[#:changed "1.26" @elem{Added the @racket[#:link-render-style] argument.}]}


@defproc[(Figure-target [tag string?]
                        [#:continue? continue? any/c #f]) 
         element?]{

Generates a new figure label. This function is normally not used
directly, since it is used by @racket[figure].}


@defproc[(suppress-floats) element?]{

Produces an empty element that renders in Latex as
@tt{\suppressfloats}, which discourages the placement of figures in
the column or page of the surrounding text.}


@section{Configuring Output}

Output uses the following style names, which can be adjusted in an
overriding @filepath{.css} or @filepath{.tex} specification:

@itemize[

 @item{@sn{Figure}, @sn{FigureMulti}, @sn{FigureMultiWide}, or
       @sn{HereFigure} --- used for the outer of three
       @racket[nested-flow]s for a figure, depending on whether
       @racket[figure], @racket[figure*], @racket[figure**], or
       @racket[figure-here] is used to generate the figure.}

 @item{@sn{Leftfigure}, @sn{Centerfigure}, or @sn{Rightfigure} ---
       used for the middle of three @racket[nested-flow]s for a
       figure, depending on the specified style.}

 @item{@sn{FigureInside} --- used for the inner of three
       @racket[nested-flow]s for a figure.}

 @item{@sn{Legend} --- Wraps the caption for a figure.}

 @item{@sn{LegendContinued} --- Wraps the caption for a figure that
       does not increment the figure counter.}

 @item{@sn{FigureTarget} --- Wraps the label anchor and text within a
       figure's caption.  For Latex output, the corresponding command
       is given a second argument, which is just the generated label
       (used with @tt{\label} in the command's first argument).}

 @item{@sn{FigureRef} --- Wraps a reference to a figure. For Latex
       output, the corresponding command is given a second argument,
       which is just the target label.}

]

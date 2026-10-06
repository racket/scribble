#lang scribble/base
@(require scribble/core)

@title[#:tag "top"]{Secref Style Modes}

@section[#:tag "numbered"]{A Numbered Section}

Body text for the numbered section.

@section[#:style '(unnumbered) #:tag "unnumbered"]{An Unnumbered Section}

Body text for the unnumbered section.

Default: @secref["numbered" #:link-render-style (link-render-style 'default)].

Number: @secref["numbered" #:link-render-style (link-render-style 'number)].

Short: @secref["numbered" #:link-render-style (link-render-style 'short)].

Number and title: @secref["numbered" #:link-render-style (link-render-style 'number-and-title)].

Short, unnumbered: @secref["unnumbered" #:link-render-style (link-render-style 'short)].

Number and title, unnumbered: @secref["unnumbered" #:link-render-style (link-render-style 'number-and-title)].

A target with no numbering metadata at all (not a section):
@(make-target-element #f (list "A Bare Target") '(part "bare-target")) is here.

Number and title, no numbering metadata: @secref["bare-target" #:link-render-style (link-render-style 'number-and-title)].

Colored short, to check the original link's style (e.g. color) survives:
@(make-link-element
  (make-style #f (list (link-render-style 'short) (make-color-property "red")))
  null
  (make-section-tag "numbered")).

@section[#:style '(unnumbered) #:tag "empty"]{}

Short, empty title (exercises the case where the fallback title is
itself empty, which the self-contained LaTeX rendering must not
mistake for another empty-content part-label link):
@secref["empty" #:link-render-style (link-render-style 'short)].

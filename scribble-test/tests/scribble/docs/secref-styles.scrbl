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

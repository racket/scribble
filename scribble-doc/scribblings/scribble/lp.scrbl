#lang scribble/doc
@(require scribble/manual scribble/core scribble/html-properties
          scribble/latex-properties
          racket/runtime-path
          racket/file
          "utils.rkt"
          (prefix-in lp-ex: "lp-ex-doc.scrbl")
          (for-label scribble/lp-include scribble/lp))

@title[#:tag "lp"
       #:style (make-style #f
                           (list (make-css-addition "lp.css")
                                 (make-tex-addition "lp.tex")))
      ]{Literate Programming}

Programs written using @racketmodname[scribble/lp2] are simultaneously
two things: a program and a document describing the program:

@itemlist[

 @item{When the program is run, all of the @racket[chunk] expressions
       are collected and stitched together into a program, and the
       rest of the module is discarded.}

 @item{When the program is provided to Scribble---or used through
       @racket[include-section] in another Scribble document with a
       @racket[(submod ... doc)] module path---the entire contents of
       the module are treated like an ordinary Scribble document,
       where @racket[chunk]s are typeset in a manner similar to
       @racket[codeblock].}

]

@(define-runtime-path lp-ex "lp-ex.rkt")

For example, consider this program:

@(codeblock (file->string lp-ex))

When this file is @racket[require]d in the normal manner, it defines a
function @racket[f] that squares its argument, and the documentation
is ignored. When it is rendered as a Scribble document, the output
looks like this:

@(make-nested-flow
  (make-style "LPBoxed" null)
  (part-blocks lp-ex:doc))

@; ------------------------------------------------------------

@section{@racketmodname[scribble/lp2] Language}

@defmodulelang[scribble/lp2 #:use-sources (scribble/lp)]{The
@racketmodname[scribble/lp] language provides core support for
literate programming. It is read like a @racketmodname[scribble/base]
program, but its bindings extend @racketmodname[scribble/base] with two
forms: @racket[chunk] and @racket[CHUNK].}

More precisely, a module in @racketmodname[scribble/lp2] has its
@racketmodname[scribble/base]-like content in a @racketidfont{doc}
submodule, which is recognized by tools such as @exec{raco scribble}.
The content of the @racket[chunk] and @racket[CHUNK] forms is
stitched together as the immediate content of the module.

The @racket[chunk] and @racket[CHUNK] content is discovered by first
@racket[expand]ing the module as written. The content is collected
into a new module, and then the original module content is placed into
a @racket[doc] submodule that is expanded (so that the content is
effectively re-expanded). The @racketidfont{doc} submodule is declared
with @racket[module*].

To include a @racketmodname[scribble/lp2] document named
 @filepath{file.scrbl} into another Scribble document,
 import the @racketidfont{doc} submodule:

@codeblock[#:keep-lang-line? #false]|{
  #lang scribble/manual
  @include-section[(submod "file.scrbl" doc)]
}|

@history[#:added "1.8"
         #:changed "1.17" @elem{Declared the @racketidfont{doc} submodule with
                                @racket[module*] instead of @racket[module].}]

@defform[(chunk id form ...)]{

  Introduces a chunk, binding @racket[id] for use in other
  chunks. Normally, @racket[id] starts with @litchar{<} and ends with
  @litchar{>}.

  When running the enclosing program, only the code inside the
  chunks is run; the rest is ignored. 

  If @racket[id] is @racketidfont{<*>}, then this chunk is
  used as the main chunk in the file. If @racketidfont{<*>}
  is never used, then the first chunk in the file is treated
  as the main chunk. If some chunk is not referenced from
  the main chunk (possibly indirectly via other chunks that
  the main chunk references), then it is not included in the
  program and thus is not run.

  The @racket[form]s are typeset using @racket[racketblock], so
  @racket[code:comment], etc., can be used to adjust the output.
  Those output-adjusting forms are stripped from each @racket[form]
  for running the program.

@history[#:changed "1.17" @elem{Strip @racket[code:comment], etc., for running.}]}

@defform[(CHUNK id form ...)]{

 Like @racket[chunk], but typesets with @racket[RACKETBLOCK], so @racket[unsyntax]
 can be used normally in each @racket[form]. To escape,
 use @racket[UNSYNTAX].

}

@; ------------------------------------------------------------

@section{@racketmodname[scribble/lp] Language}

@defmodulelang[scribble/lp]{Programs written using the older
@racketmodname[scribble/lp] language are similar to
@racketmodname[scribble/lp2] programs, except that the module cannot
be provided directly to Scribble. Instead, the document content must be
extracted using @racket[lp-include].}

The @racketmodname[scribble/lp] language effectively binds only
@racket[chunk] and @racket[CHUNK], while all other bindings for
documentation are taken from the context where @racket[lp-include] is
used.

@; ------------------------------------------------------------

@section{@racketmodname[scribble/lp-include] Module}

@defmodule[scribble/lp-include]{The
@racketmodname[scribble/lp-include] library is normally used within a
Scribble document---that is, a module that starts with something like
@hash-lang[] @racketmodname[scribble/base] or @hash-lang[] @racketmodname[scribble/manual],
instead of @hash-lang[] @racketmodname[racket].}

@defform[(lp-include filename)]{
Includes the source of @racket[filename] as the typeset version of the literate
program.
}

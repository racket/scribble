#lang scribble/doc
@(require scribble/manual
          "utils.rkt"
          (for-label scribble/eval
                     racket/sandbox
                     racket/pretty
                     file/convertible
                     racket/serialize))

@(define-syntax-rule (define-new-examples new-examples)
   (begin
     (require (for-label scribble/example))
     (define new-examples @racket[examples])))
@(define-new-examples new-examples)


@title[#:tag "old-eval"]{Legacy Evaluation}

@defmodule[scribble/eval]{The @racketmodname[scribble/eval] library provides
an older interface to the functionality of @racketmodname[scribble/example].
The @racketmodname[scribble/example] library should be used, instead.}

In addition to the forms listed below, @racket[scribble/eval]
re-exports several functions from @racket[scribble/example]:
@racket[make-base-eval] @racket[make-base-eval-factory],
@racket[make-eval-factory], @racket[make-log-based-eval],
@racket[close-eval], and @racket[scribble-eval-handler].


@defform/subs[(interaction maybe-options datum ...)
              ([maybe-options maybe-eval maybe-escape maybe-no-errors]
               [maybe-eval code:blank
                            (code:line #:eval eval-expr)]
               [maybe-escape code:blank
                            (code:line #:escape escape-id)]
               [maybe-no-errors code:blank
                                (code:line #:no-errors? no-errors?-expr)])]{

Like @|new-examples| from @racketmodname[scribble/example], except that

@itemlist[

 @item{the ``Examples:'' label is always supressed,}

 @item{exceptions raised during the evaluation of a @racket[datum] are
       always rendered as errors, unless @racket[#:no-errors?] is
       specified with a true value; and}

 @item{the @racket[#:once] option is never implicitly used.}

]

@history[#:changed "1.14" @elem{Added @racket[#:no-errors?],
                                @racket[eval:no-prompt], and
                                @racket[eval:error], and changed
                                @racket[code:line] to support multiple
                                @racket[_datum]s.}]}


@defform[(interaction0 maybe-options datum ...)]{
Like @racket[interaction], but without insetting the code via
@racket[nested].

Use @|new-examples| with @racket[#:no-indent], instead.}


@defform[(interaction/no-prompt maybe-eval maybe-escape datum)]{
  Like @racket[interaction], but does not render each @racket[datum] with a prompt.

 Use @|new-examples| with @racket[#:no-prompt], instead.}
 

@defform[(interaction-eval maybe-eval datum)]{

Like @racket[interaction], evaluates the @racket[quote]d form of
@racket[datum], but returns the empty string and does not catch
exceptions (so @racket[eval:error] has no effect).

Use @|new-examples| with @racket[#:hidden], instead.}


@defform[(interaction-eval-show maybe-eval datum)]{

Like @racket[interaction-eval], but produces an element representing
the printed form of the evaluation result.

Use @|new-examples| with @racket[#:result-only], instead.}


@defform[(racketblock+eval maybe-eval maybe-escape datum ...)]{

Combines @racket[racketblock] and @racket[interaction-eval].

Use @|new-examples| with @racket[#:no-result], instead.}


@defform[(racketblock0+eval maybe-eval maybe-escape datum ...)]{

Combines @racket[racketblock0] and @racket[interaction-eval].

Use @|new-examples| with @racket[#:no-result] and
@racket[#:no-indent], instead.}


@defform[(racketmod+eval maybe-eval maybe-escape name datum ...)]{

Combines @racket[racketmod] and @racket[interaction-eval].

Use @|new-examples| with @racket[#:lang], instead.}


@defform[(def+int maybe-options defn-datum expr-datum ...)]{

Like @racket[interaction], except the @racket[defn-datum] is
typeset as for @racket[racketblock] (i.e., no prompt) and a line of
space is inserted before the @racket[expr-datum]s.}


@defform[(defs+int maybe-options (defn-datum ...) expr-datum ...)]{

Like @racket[def+int], but for multiple leading definitions.

Use @|new-examples| with @racket[eval:no-prompt] wrappers on
definitions, instead.}


@defform[(examples maybe-options datum ...)]{

Like @racket[interaction], but with an ``Examples:'' label prefixed.

Use @|new-examples| from @racketmodname[scribble/example], instead.}


@defform[(examples* label-expr maybe-options datum ...)]{

Like @racket[examples], but using the result of @racket[label-expr] in
place of the default ``Examples:'' label.

Use @|new-examples| from @racketmodname[scribble/example] with the
@racket[#:label] option, instead.}


@defform[(defexamples maybe-options datum ...)]{

Like @racket[examples], but each definition using @racket[define] or
@racket[define-struct] among the @racket[datum]s is typeset without a
prompt, and with line of space after it.

Use @|new-examples| with @racket[eval:no-prompt] wrappers on
definitions, instead.}


@defform[(defexamples* label-expr maybe-options datum ...)]{

Like @racket[defexamples], but using the result of @racket[label-expr] in
place of the default ``Examples:'' label.

Use @|new-examples| with the @racket[#:label] option and
@racket[eval:no-prompt] wrappers on definitions, instead.}


@defproc*[([(as-examples [b block?]) block?]
           [(as-examples [label (or/c block? content?)]
                         [b block?])
            block?])]{

Adds an ``examples'' label to @racket[b], using either a default label
or the given @racket[label].}

@defform[(with-eval-preserve-source-locations expr ...)]{

By default, the evaluation forms provided by this module, such as
@racket[interaction] and @racket[examples], discard the source
locations from the expressions they evaluate. Within a
@racket[with-eval-preserve-source-locations] form, the source
locations are preserved. This can be useful for documenting forms that
depend on source locations, such as Redex's typesetting macros.

Use @|new-examples| with the @racket[#:preserve-source-locations]
option, instead.}

#lang scribble/doc
@(require scribble/manual
          "utils.rkt"
          (for-label scribble/example
                     racket/sandbox
                     racket/pretty
                     file/convertible
                     racket/serialize))

@title[#:tag "eval"]{Evaluation and Examples}

@defmodule[scribble/example #:use-sources (scribble/eval scribble/example)]{The
@racket[scribble/example] library provides
utilities for evaluating code at document-build time and incorporating
the results in the document, especially to show example uses of
defined procedures and syntax.}

@history[#:added "1.16"]

@defform/subs[(examples option ... datum ...)
              ([option (code:line #:eval eval-expr)
                       #:once
                       (code:line #:escape escape-id)
                       (code:line #:label label-expr)
                       #:hidden
                       #:result-only
                       #:no-inset
                       #:no-prompt
                       #:preserve-source-locations
                       #:no-result
                       (code:line #:lang language-name)])]{

Similar to @racket[racketinput], except that the result for each input
@racket[datum] is shown on the next line. The result is determined by
evaluating the @racket[quote]d form of the @racket[datum] using the
evaluator produced by @racket[eval-expr].

Each keyword option can be provided at most once:

@itemlist[

 @item{@racket[#:eval eval-expr] --- Specifies an evaluator, where
       @racket[eval-expr] must produce either @racket[#f] or a sandbox
       evaluator via @racket[make-evaluator] or
       @racket[make-module-evaluator] with the @racket[sandbox-output]
       and @racket[sandbox-error-output] parameters set to
       @racket['string]. If @racket[eval-expr] is not provided or is
       @racket[#f], an evaluator is created using
       @racket[make-base-eval]. See also @racket[make-eval-factory].}

  @item{@racket[#:once] --- Specifies that the evaluator should be
        closed with @racket[close-eval] after the all @racket[datum]s
        are evaluated. The @racket[#:once] option is assumed if
        @racket[#:eval] is not specified.}

  @item{@racket[@#,racket[#:escape] escape-id] --- Specifies an escape
        identifier, as in @racket[racketblock].}

  @item{@racket[#:label label-expr] --- Specifies a label for the
        examples, which defaults to ``Example:'' or ``Examples:''
        (depending on the number of @racket[datum]s). A @racket[#f]
        value for @racket[label-expr] suppresses the label.}

  @item{@racket[#:hidden] --- Specifies that the @racket[datum]s and
        results should not be typeset, but instead evaluated for a
        side-effect, and disables @racket[eval:error]. Typically, this
        option is combined with @racket[#:eval] to configure an
        evaluator.}

  @item{@racket[#:result-only] --- Specifies that the @racket[datum]
        results should be typeset, but not the @racket[datum]s
        themselves, and implies @racket[#:label #f].}

  @item{@racket[#:no-result] --- Implies @racket[#:no-prompt] and
        @racket[#:label #f], specifies that no results should be
        typeset, and disables @racket[eval:error].}

  @item{@racket[#:no-inset] --- Specifies that the examples should be
        typeset without indentation, i.e., like @racket[racketinput0]
        instead of @racket[racketinput].}
        
  @item{@racket[#:no-prompt] --- Specifies that each examples should
        be typeset without a leading prompt, i.e., like
        @racket[racketblock] instead of @racket[racketinput]. A prompt
        can be omitted from a specific @racket[_datum] by wrapping it
        with @racket[eval:no-prompt].}

  @item{@racket[#:preserve-source-locations] --- Specifies that the
        original source locations for each @racket[datum] should be
        preserved for evaluation. Preserving source locations can be
        useful for documenting forms that depend on source locations,
        such as Redex's typesetting macros.}

  @item{@racket[#:lang] --- Implies @racket[#:no-result] prefixes the
        typeset @racket[datum] sequence with a @hash-lang[] line using
        @racket[language-name] as the module's language.}

]

Certain patterns in @racket[datum] are treated specially:

@itemlist[

 @item{A @racket[datum] of the form 
       @racket[(@#,indexed-racket[code:line] _code-datum (@#,racketidfont{code:comment} _comment-datum ...))]
       is treated as @racket[_code-datum] for evaluation.}

@item{A @racket[datum] of the form 
       @racket[(@#,indexed-racket[code:line] _code-datum ...)]
       evaluates each @racket[_code-datum], but only the last result is used.}

 @item{Other uses of @racketidfont{code:comment}, @racketidfont{code:contract}, and
       @racketidfont{code:blank} are stripped from each @racket[datum]
       before evaluation.}

 @item{A @racket[datum] of the form
       @racket[(@#,indexed-racket[eval:error] #,(svar eval-datum))] is
       treated like @racket[_eval-datum], but @racket[_eval-datum] is
       expected to raise an exception, and an error is shown as the
       evaluation's result.}

 @item{A @racket[datum] of the form 
       @racket[(@#,indexed-racket[eval:alts] #,(svar show-datum) #,(svar eval-datum))]
       is treated as @svar[show-datum] for typesetting and @svar[eval-datum] for evaluation.}

 @item{A @racket[datum] of the form 
       @racket[(@#,indexed-racket[eval:check] #,(svar eval-datum) #,(svar expect-datum))]
       is treated like @racket[_eval-datum], but @svar[check-datum] is also
       evaluated, and an error is raised if they are not @racket[equal?].}

 @item{A @racket[datum] of the form 
       @racket[(@#,indexed-racket[eval:result] _content-expr _out-expr _err-expr)]
       involves no sandboxed evaluation; instead, the @tech{content} result of @racket[_content-expr] is used as the
       typeset form of the result, @racket[_out-expr] is treated as output printed
       by the expression, and @racket[_err-expr] is error output printed by the
       expression. The @racket[_out-expr] and/or @racket[_err-expr] can be omitted,
       in which case they default to empty strings.

       Normally, @racketidfont{eval:result}
       is used in the second part of an @racketidfont{eval:alts} combination. Otherwise,
       @racket[_content-expr] is typeset as the input form (which rarely makes sense for
       a reader of the example).}

 @item{A @racket[datum] of the form 
       @racket[(@#,indexed-racket[eval:results] _content-list-expr _out-expr _err-expr)]
       is treated like an @racketidfont{eval:result} form, except that @racket[_content-list-expr]
       should produce a list of @tech{content} for multiple results of evaluation. As
       with @racketidfont{eval:result}, @racket[_out-expr] and @racket[_err-expr] are optional.}

 @item{A @racket[datum] of the form 
       @racket[(@#,indexed-racket[eval:no-prompt] _eval-datum ...)]
       is treated like @racket[(@#,racket[code:line] _eval-datum ...)], but no prompt is shown before
       the group, and a blank line is added before and after
       @(svar eval-datum) and its result.}

]

A @racket[datum] cannot be a keyword. To specify a @racket[datum] that
is a keyword, wrap it with @racket[code:line].

When evaluating a @racket[datum] produces an error (and @racket[datum]
does not have an @racket[eval:error] wrapper), an exception is raised
by @racket[examples].

If the value of @racket[current-print] in the sandbox is changed from
its default value, or if @racket[print-as-expression] in the sandbox
is set to @racket[#f], then each evaluation result is formatted to a
port by applying @racket[(current-print)] to the value; the output
port is set to a pipe that supports specials in the sense of
@racket[write-special], and non-character values written to the port
are used as @tech{content}. Otherwise, when the default
@racket[current-print] is in place, result values are typeset using
@racket[to-element/no-color].

As an example,

@codeblock|{
#lang scribble/manual
@(require racket/sandbox
          scribble/eval)
@(define my-evaluator
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator 'typed/racket/base)))

@examples[#:eval my-evaluator
          (: my-sqr (Real -> Real))
          (define (my-sqr x)
            (* x x))
          (my-sqr 42)]
}|

uses an evaluator whose language is @racketmodname[typed/racket/base].}


@defproc[(make-base-eval [#:pretty-print? pretty-print? any/c #t]
                         [#:lang lang
                          (or/c module-path?
                                (list/c 'special symbol?)
                                (cons/c 'begin list?))
                          '(begin)]
                         [input-program any/c] ...)
         (any/c . -> . any)]{

Creates an evaluator using @racket[(make-evaluator 'racket/base #:lang lang input-program ...)],
setting sandbox parameters to disable limits, setting the outputs to
@racket['string], and not adding extra security guards.

If @racket[pretty-print?] is true, the sandbox's printer is set to
@racket[pretty-print-handler]. In that case, values that are convertible
in the sense of @racket[convertible?] are printed using @racket[write-special],
except that values that are serializable in the sense of @racket[serializable?]
are serialized for tranfers from inside the sandbox to outside (which can avoid
pulling code and support from the sandboxed environment into the document-rendering
environment).

@history[#:changed "1.6" @elem{Changed treatment of convertible values that are
                               serializable.}]}


@defproc[(make-base-eval-factory [mod-paths (listof module-path?)]
                                 [#:pretty-print? pretty-print? any/c #t]
                                 [#:lang lang
                                  (or/c module-path?
                                        (list/c 'special symbol?)
                                        (cons/c 'begin list?))
                                  '(begin)])
         (-> (any/c . -> . any))]{

Produces a function that is like @racket[make-base-eval], except that
each module in @racket[mod-paths] is attached to the evaluator's
namespace. The modules are loaded and instantiated once (when the
returned @racket[make-base-eval]-like function is called the first
time) and then attached to each evaluator that is created.}


@defproc[(make-eval-factory [mod-paths (listof module-path?)]
                            [#:pretty-print? pretty-print? any/c #t]
                            [#:lang lang
                             (or/c module-path?
                                   (list/c 'special symbol?)
                                   (cons/c 'begin list?))
                             '(begin)])
         (-> (any/c . -> . any))]{

Like @racket[make-base-eval-factory], but each module in @racket[mod-paths] is
also required into the top-level environment for each generated evaluator.}


@defproc[(make-log-based-eval [log-file path-string?]
                              [mode (or/c 'record 'replay)])
         (-> any/c any)]{

Creates an evaluator (like @racket[make-base-eval]) that uses a log
file to either record or replay evaluations.

If @racket[mode] is @racket['record], the evaluator records every
interaction to @racket[log-file], replacing @racket[log-file] if it
already exists. The result of each interaction must be
@seclink["serialization" #:doc '(lib
"scribblings/reference/reference.scrbl")]{serializable}.

If @racket[mode] is @racket['replay], the evaluator uses the contents
of @racket[log-file] instead of actually performing evaluatings. For
each interaction, it compares the term to evaluate against the next
interaction recorded in @racket[log-file]. If the term matches, the
stored result is returned; if not, the evaluator raises an error
indicating that it is out of sync with @racket[log-file].

Use @racket[make-log-based-eval] to document libraries when the
embedded examples rely on external features that may not be present or
appropriately configured on all machines.

@history[#:added "1.12"]}


@defproc[(close-eval [eval (any/c . -> . any)]) (one-of/c "")]{

Shuts down an evaluator produced by @racket[make-base-eval]. Use
@racket[close-eval] when garbage collection cannot otherwise reclaim
an evaluator (e.g., because it is defined in a module body).}


@defparam[scribble-eval-handler handler 
          ((any/c . -> . any) any/c boolean? . -> . any)]{

A parameter that serves as a hook for evaluation. The evaluator to use
is supplied as the first argument to the parameter's value, and the
second argument is the form to evaluate. The last argument is
@racket[#t] if exceptions are being captured (to display exception
results), @racket[#f] otherwise.}

@defparam[scribble-exn->string handler (-> (or/c exn? any/c) string?)]{
  A parameter that controls how exceptions are rendered by 
  @racket[interaction]. Defaults to
  @racketblock[(λ (e)
                 (if (exn? e)
                     (exn-message e)
                     (format "uncaught exception: ~s" e)))]
}

@; ------------------------------------------------------------

@include-section["eval.scrbl"]

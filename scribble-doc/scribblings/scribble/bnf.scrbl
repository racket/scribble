#lang scribble/doc
@(require scribble/manual "utils.rkt" scribble/bnf
          ;; "utils.rkt" provides scribble/bnf for-label
          )

@title[#:tag "bnf"]{BNF Grammars}

@defmodule[scribble/bnf]{The @racket[scribble/bnf] library
provides utilities for typesetting grammars.}

For example,

@verbatim[#:indent 2]|{
@(let ([open @litchar{(}]
       [close @litchar{)}])
   @BNF[(list @nonterm{expr}
              @nonterm{id}
              @BNF-seq[open @kleeneplus[@nonterm{expr}] close]
              @BNF-seq[open @litchar{lambda}
                        open @kleenestar[@nonterm{id}] close
                        @nonterm{expr} close]
              @nonterm{val})
        (list @nonterm{val} 
              @BNF-alt[@nonterm{number} @nonterm{primop}])
        (list @nonterm{id}
              @elem{any name except for @litchar{lambda}})])
}|

produces the output

@(let ([open @litchar{(}]
       [close @litchar{)}])
   @BNF[(list @nonterm{expr}
              @nonterm{id}
              @BNF-seq[open @kleeneplus[@nonterm{expr}] close]
              @BNF-seq[open @litchar{lambda}
                        open @kleenestar[@nonterm{id}] close
                        @nonterm{expr} close]
            @nonterm{val})
      (list @nonterm{val} 
            @BNF-alt[@nonterm{number} @nonterm{primop}])
        (list @nonterm{id}
              @elem{any name except for @litchar{lambda}})])

See also @racket[racketgrammar].

@defproc[(BNF [prod (cons/c (or/c block? content?)
                            (non-empty-listof (or/c block? content?)))]
              ...)
         table?]{

Typesets a grammar table. Each production starts with an element
(typically constructed with @racket[nonterm]) for the non-terminal
being defined, and then a list of possibilities (typically constructed
with @racket[BNF-seq], etc.) to show on separate lines.}

@defproc[(nonterm [pre-content pre-content?] ...) element?]{

Typesets a non-terminal: italic in angle brackets.}

@defproc[(BNF-seq [elem content?] ...) (or/c element? "")]{

Typesets a sequence.}

@defproc[(BNF-seq-lines [elems (listof content?)] ...) block?]{

Typesets a sequence that is broken into multiple lines, where each
@racket[elems] is one line.}

@defproc[(BNF-group [pre-content pre-content?] ...) element?]{

Typesets a group surrounded by curly braces (so the entire group can
be repeated, for example).}

@defproc[(optional [pre-content pre-content?] ...) element?]{

Typesets an optional element: in square brackets.}

@defproc[(kleenestar [pre-content pre-content?] ...) element?]{

Typesets a 0-or-more repetition.}

@defproc[(kleeneplus [pre-content pre-content?] ...) element?]{

Typesets a 1-or-more repetition.}

@defproc[(kleenerange [n any/c] [m any/c] [pre-content pre-content?] ...)
         element?]{

Typesets a @racket[n]-to-@racket[m] repetition. The @racket[n] and
@racket[m] arguments are converted to a string using @racket[(format
"~a" n)] and @racket[(format "~a" m)].}

@defproc[(BNF-alt [elem element?] ...) element?]{

Typesets alternatives for a production's right-hand side to appear on
a single line. The result is normally used as a single possibility in
a production list for @racket[BNF].}

@; BNF-alt/close is exported but undocumented.
@; It looks like it produces a more densely packed version of
@; BNF-alt, but I haven't confirmed this.

@defthing[BNF-etc element?]{

An element to use for omitted productions or content.
Renders as: @BNF-etc
}



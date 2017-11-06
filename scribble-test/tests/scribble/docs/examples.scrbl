#lang scribble/base
@(require scribble/examples)

@(define shared-eval (make-base-eval))
@examples[#:hidden #:eval shared-eval "just testing"]

@examples[
(+ 1 2)
]

@examples[
(+ 3 4)
(string-append "5"
               "6")
]

@examples[
#:label #f
(+ 2 3)
]

@examples[
#:label "Another example:"
(+ 2 4)
]

@examples[
#:no-inset
(+ 2 5)
(* 3 4)
]

@examples[
#:no-prompt
(+ 2 6)
(* 3 5)
]

@examples[
#:result-only
"Just the result."]

@examples[
#:preserve-source-locations
(syntax-line (quote-syntax here))
]

@examples[
#:no-result
"don't show the result"
]

@examples[
#:lang racket/base
(define x "don't show")
"the result"
]

@examples[
(eval:error (/ 1 0))
]

@examples[
(code:line (+ 1 2) (* 3 4))
]

@examples[
(eval:check (+ 1 2) (* 3 1))
]

@examples[
(eval:alts (/ 1 0) +inf.0)
]

@examples[
(eval:result "(/ 1 0)")
]

@examples[
(eval:result "(/ 1 0)" "getting +inf.0")
]

@examples[
(eval:result "(/ 1 0)" "getting +inf.0" "oops")
]

@examples[
(eval:alts (/ 100 0) (eval:result "(/ 1 0)" "getting +inf.0" "oops"))
]

@examples[
(eval:alts (/ 100 0) (eval:results (list "(/ 1 0)" "'=") "getting +inf.0" "oops"))
]

@examples[
#:no-prompt
(eval:alts (define x 42) (define x 42))
]

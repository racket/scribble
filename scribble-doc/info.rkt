#lang info

(define collection 'multi)

(define build-deps '("racket-index"
                     "mzscheme-doc"
                     "net-doc"
                     "scheme-lib"
                     "draw-doc"
                     "gui-doc"
                     "slideshow-doc"
                     "pict-doc"
                     "typed-racket-doc"
                     "at-exp-lib"
                     "base"
                     "compatibility-lib"
                     "draw-lib"
                     "pict-lib"
                     "sandbox-lib"
                     "slideshow-lib"
                     "scribble-lib"
                     "scribble-text-lib"
                     "racket-doc"))
(define update-implies '("scribble-lib"))

(define pkg-desc "documentation part of \"scribble\"")

(define pkg-authors '(mflatt eli))

(define license
  '(Apache-2.0 OR MIT))

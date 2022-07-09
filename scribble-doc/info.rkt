#lang info

(define collection 'multi)

(define build-deps '("racket-index"
                     "net-doc"
                     "scheme-lib"
                     "draw-doc"
                     "at-exp-lib"
                     "base"
                     "compatibility-lib"
                     "draw-lib"
                     "pict-lib"
                     "pict-doc"
                     "sandbox-lib"
                     "scribble-lib"
                     "scribble-text-lib"
                     "racket-doc"))
(define update-implies '("scribble-lib"))

(define pkg-desc "documentation part of \"scribble\"")

(define pkg-authors '(mflatt eli))

(define license
  '(Apache-2.0 OR MIT))

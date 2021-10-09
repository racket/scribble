#lang info

(define collection 'multi)

(define deps '("at-exp-lib"
               "base"
               "eli-tester"
               "rackunit-lib"
               "sandbox-lib"
               "scribble-doc" ;; because some tests are examples from the docs
               "scribble-lib"
               "scribble-text-lib"))

(define pkg-desc "tests for \"scribble\"")

(define pkg-authors '(mflatt eli))
(define build-deps '("racket-index"))
(define update-implies '("scribble-lib"))

(define license
  '(Apache-2.0 OR MIT))

#lang info

(define collection 'multi)

(define deps '("scribble-lib"
               "scribble-doc"))
(define implies '("scribble-lib"
                  "scribble-doc"))

(define pkg-desc "Racket documentatation and typesetting tool")

(define pkg-authors '(mflatt eli))

(define license
  '(Apache-2.0 OR MIT))

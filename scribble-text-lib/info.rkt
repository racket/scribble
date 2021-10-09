#lang info

(define collection 'multi)

(define deps '("scheme-lib"
               ["base" #:version "8.2.0.7"]
               "at-exp-lib"))

(define pkg-desc "Language for text with embedded Racket code")

(define pkg-authors '(mflatt eli))

(define version "1.1")

(define license
  '(Apache-2.0 OR MIT))

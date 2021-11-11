#lang racket/base

(require (only-in scribble/reader make-at-readtable)
         racket/interaction-info)

(define old-read (current-read-interaction))
(define (scribble-read-interaction src in)
  (parameterize ([current-readtable (make-at-readtable #:readtable (current-readtable))])
    (old-read src in)))
(current-read-interaction scribble-read-interaction)
(current-interaction-info
 '#(scribble/base/reader
    scribble-interaction-info
    #f))

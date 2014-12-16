#lang racket/base
(require scribble/private/lp)
(provide chunk CHUNK)

(module reader syntax/module-reader
  scribble/lp/lang/lang2

  #:read read-inside
  #:read-syntax read-syntax-inside
  #:whole-body-readers? #t
  #:language-info (scribble-base-language-info)
  #:info (scribble-base-info)
  (require scribble/reader
           (only-in scribble/base/reader
                    scribble-base-info
                    scribble-base-language-info)))

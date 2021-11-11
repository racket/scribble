#lang racket/base
(require scribble/doclang scribble/base)
(provide (all-from-out scribble/doclang
                       scribble/base))
(module configure-runtime racket/base (require scribble/base/lang/configure-runtime))

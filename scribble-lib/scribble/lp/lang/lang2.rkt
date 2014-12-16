#lang racket/base
(require "common.rkt")

(provide (except-out (all-from-out "common.rkt")
                     module-begin/plain
                     module-begin/doc)
         (rename-out [module-begin/doc #%module-begin]))

#lang racket/base

(require tests/eli-tester
         "docs.rkt"
         "markdown.rkt"
         "reader.rkt"
         "render.rkt"
         "text-collect.rkt"
         "text-lang.rkt"
         "text-wrap.rkt"
         "xref.rkt")

(test do (reader-tests)
      do (begin/collect-tests)
      do (text-lang-tests)
      do (wrap-tests)
      do (docs-tests)
      do (render-tests)
      do (xref-tests)
      do (markdown-tests))

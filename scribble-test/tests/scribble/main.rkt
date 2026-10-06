#lang racket/base

(require tests/eli-tester
         "reader.rkt" "text-collect.rkt" "text-lang.rkt" "text-wrap.rkt"
         "docs.rkt" "render.rkt" "xref.rkt" "markdown.rkt" "typst.rkt"
         "secref-styles.rkt")

(test do (reader-tests)
      do (begin/collect-tests)
      do (text-lang-tests)
      do (wrap-tests)
      do (docs-tests)
      do (render-tests)
      do (xref-tests)
      do (markdown-tests)
      do (typst-tests)
      do (secref-styles-tests))

#lang scheme/base
(require (for-syntax scheme/base))

(provide quote-syntax/loc)

;; Source locations are now preserved in the bytecode form of `quote-syntax`:
(define-syntax-rule (quote-syntax/loc d)
  (quote-syntax d))

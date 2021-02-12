#lang racket/base
(require "../struct.rkt")

; XXX unknown contracts
(provide (struct-out exporting-libraries)
         current-signature)

(struct exporting-libraries element (libs source-libs pkgs)
  #:extra-constructor-name make-exporting-libraries)

(define current-signature (make-parameter #f))

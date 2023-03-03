#lang racket/base
(require "../struct.rkt"
         "../base.rkt"
         (only-in "../core.rkt"
                   content?
                   style?)
         racket/contract/base
         racket/list)

(provide doc-prefix)
(provide/contract
 [spacer element?]
 [to-flow (content? . -> . flow?)]
 [flow-spacer flow?]
 [flow-spacer/n (-> exact-nonnegative-integer? flow?)]
 [flow-empty-line flow?]
 [make-table-if-necessary ((or/c style? string?) list? . -> . (list/c (or/c omitable-paragraph? table?)))]
 [current-display-width (parameter/c exact-nonnegative-integer?)])

(define spacer (hspace 1))

(define (to-flow e)
  (make-flow (list (make-omitable-paragraph (list e)))))
(define flow-spacer (to-flow spacer))
(define (flow-spacer/n n) (to-flow (hspace n)))
(define flow-empty-line (to-flow (tt 'nbsp)))

(define (make-table-if-necessary style content)
  (cond
    [(= 1 (length content))
     (define paras (append-map flow-paragraphs (car content)))
     (if (andmap paragraph? paras)
         (list (make-omitable-paragraph (append-map paragraph-content paras)))
         (list (make-table style content)))]
    [else (list (make-table style content))]))

(define current-display-width (make-parameter 65))

#lang racket/base

(require (for-syntax racket/base)
         scribble/core
         (only-in scribble/base superscript)
         (only-in scriblib/render-cond cond-element)
         scribble/decode
         scribble/html-properties
         scribble/latex-properties
         racket/promise
         setup/main-collects
         scriblib/render-cond
         "private/counter.rkt")

(provide note
         note-number
         define-footnote)

(define footnote-style-extras
  (let ([abs (lambda (s)
               (path->main-collects-relative
                (collection-file-path s "scriblib")))])
    (list (make-css-addition (abs "footnote.css"))
          (make-tex-addition (abs "footnote.tex")))))


(define note-box-style (make-style "NoteBox" footnote-style-extras))
(define note-content-style (make-style "NoteContent" footnote-style-extras))

(define note-number (make-parameter #f))

;; TODO: move this utility function somewhere it can be exported from
(define (xexpr-element xexpr)
  (make-element (make-style #f (list (xexpr-property xexpr ""))) '()))

(define (note #:number [number (note-number)] . text)
  (define (no-number)
    (make-element
      note-box-style
      (make-element note-content-style
                    (decode-content text))))
  (cond-element
    [html
      (if number
        (let* ([n (if (integer? number)
                    number
                    (let ([nn (note-number)])
                      (if (integer? nn) nn 1)))]
               [a (lambda (x y)
                    (xexpr-element `[a ([name ,(format "__footnote_~a_~a__" x n)]
                                        [href ,(format "#__footnote_~a_~a__" y n)])
                                       [sup () ,(format "~a" n)]]))])
          (note-number (+ n 1))
          (make-element plain
            (list
              (a "source" "target")
              (make-element
                note-box-style
                (make-element note-content-style
                  (list
                    (a "target" "source")
                    ": "
                    (decode-content text)))))))
        (no-number))]
    [else
      (no-number)]))

(define footnote-style (make-style "Footnote" footnote-style-extras))
(define footnote-ref-style (make-style "FootnoteRef" footnote-style-extras))
(define footnote-ref-number-style (make-style "FootnoteRefNumber" footnote-style-extras))
(define footnote-content-style (make-style "FootnoteContent" footnote-style-extras))
(define footnote-margin-content-style (make-style "FootnoteMarginContent" footnote-style-extras))
(define footnote-target-style (make-style "FootnoteTarget" footnote-style-extras))
(define footnote-target-number-style (make-style "FootnoteTargetNumber" footnote-style-extras))
(define footnote-block-style (make-style "FootnoteBlock" footnote-style-extras))
(define footnote-block-content-style (make-style "FootnoteBlockContent" footnote-style-extras))

(define-syntax (define-footnote stx)
  (define (check-identifier id)
    (unless (identifier? id)
      (raise-syntax-error #f "expected an identifier" stx id)))
  (define (generate-footnote footnote-id margin?)
    #`(begin
        (define footnotes (new-counter "footnote"))
        (define id (gensym))
        (define (#,footnote-id . text) (do-footnote footnotes id text #,margin?))))
  (syntax-case stx ()
    [(_ footnote #:margin)
     (begin
       (check-identifier #'footnote)
       (generate-footnote #'footnote #t))]
    [(_ footnote footnote-part)
     (begin
       (check-identifier #'footnote)
       (check-identifier #'footnote-part)
       #`(begin
           #,(generate-footnote #'footnote #f)
           (define (footnote-part . text) (do-footnote-part footnotes id))))]))

(define (do-footnote footnotes id text margin?)
  (define tag (generated-tag))
  (define content (decode-content text))
  (define target (cons (make-element footnote-target-style
                                     (make-element footnote-target-number-style
                                                   (counter-target footnotes tag #f
                                                                   #:use-ref? #t)))
                       content))
  (make-traverse-element
   (lambda (get set)
     (unless margin?
       (set id (cons target (get id null))))
     (make-element footnote-style
                   (list (make-element footnote-ref-style
                                       (make-element footnote-ref-number-style
                                                     (counter-ref footnotes tag #f
                                                                  #:use-target? #t)))
                         (if margin?
                             (make-element footnote-margin-content-style target)
                             (cond-element
                              [latex
                               (make-element footnote-content-style target)]
                              [else
                               null])))))))

(define (do-footnote-part footnotes id)
  (make-part
   #f
   (list `(part ,(generated-tag)))
   #f
   (make-style #f '(unnumbered hidden toc-hidden))
   null
   (list
    (make-traverse-block
     (lambda (get set)
       (make-compound-paragraph
        footnote-block-style
        (for/list ([content (in-list (reverse (get id null)))])
          (make-paragraph footnote-block-content-style
                          (cond-element
                           [latex null]
                           [else content])))))))
   null))

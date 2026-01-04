#lang racket/base

(require scribble/core
         (only-in scribble/base superscript)
         (only-in scriblib/render-cond cond-element)
         scribble/decode
         scribble/html-properties
         scribble/latex-properties
         racket/promise
         setup/main-collects
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
               [f (lambda (s d)
                    (define a `(a ([name ,(format "~a~a" s n)] [href ,(format "#~a~a" d n)])
                                  [sup () ,(format "~a" n)]))
                    (make-element (make-style #f (list (xexpr-property a ""))) '()))])
          (note-number (+ n 1))
          (make-element plain
            (list
              (f "__footnote_src_" "__footnote_dst_")
              (make-element
                note-box-style
                (make-element note-content-style
                  (list
                    (f "__footnote_dst_" "__footnote_src_")
                    ": "
                    (decode-content text)))))))
        (no-number))]
    [else
      (no-number)]))

(define footnote-style (make-style "Footnote" footnote-style-extras))
(define footnote-ref-style (make-style "FootnoteRef" footnote-style-extras))
(define footnote-content-style (make-style "FootnoteContent" footnote-style-extras))
(define footnote-target-style (make-style "FootnoteTarget" footnote-style-extras))
(define footnote-block-style (make-style "FootnoteBlock" footnote-style-extras))
(define footnote-block-content-style (make-style "FootnoteBlockContent" footnote-style-extras))

(define-syntax-rule (define-footnote footnote footnote-part)
  (begin
    (define footnotes (new-counter "footnote"))
    (define id (gensym))
    (define (footnote . text) (do-footnote footnotes id text))
    (define (footnote-part . text) (do-footnote-part footnotes id))))

(define (do-footnote footnotes id text)
  (define tag (generated-tag))
  (define content (decode-content text))
  (make-traverse-element
   (lambda (get set)
     (set id
          (cons (cons (make-element footnote-target-style
                                    (make-element 'superscript (counter-target footnotes tag #f)))
                      content)
                (get id null)))
     (make-element footnote-style
                   (list (make-element footnote-ref-style
                                       (make-element 'superscript (counter-ref footnotes tag #f)))
                         (make-element footnote-content-style content))))))

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
          (make-paragraph footnote-block-content-style content))))))
   null))

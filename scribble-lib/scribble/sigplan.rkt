#lang racket/base
(require (for-syntax racket/base)
         racket/contract/base
         scribble/base
         scribble/core
         scribble/decode
         scribble/html-properties
         scribble/latex-properties
         setup/collects)

(provide/contract
 [abstract 
  (->* () () #:rest (listof pre-content?)
       block?)]
 [subtitle
  (->* () () #:rest (listof pre-content?)
       content?)]
 [authorinfo
  (-> pre-content? pre-content? pre-content?
      block?)]
 [conferenceinfo
  (-> pre-content? pre-content?
      block?)]
 [copyrightyear
  (->* () () #:rest (listof pre-content?)
       block?)]
 [copyrightdata
  (->* () () #:rest (listof pre-content?)
       block?)]
 [exclusive-license
  (->* () ()
       block?)]
 [doi
  (->* () () #:rest (listof pre-content?)
       block?)]
 [to-appear
  (->* () () #:rest pre-content?
       block?)]
 [category
  (->* (pre-content? pre-content? pre-content?)
       ((or/c #f pre-content?))
       content?)]
 [terms
  (->* () () #:rest (listof pre-content?)
       content?)]
 [keywords
  (->* () () #:rest (listof pre-content?)
       content?)])

(provide preprint 10pt nocopyright onecolumn noqcourier notimes
         include-abstract)

(define-syntax-rule (defopts name ...)
  (begin (define-syntax (name stx)
           (raise-syntax-error #f
                               "option must appear on the same line as `#lang scribble/sigplan'"
                               stx))
         ...
         (provide name ...)))
(defopts preprint 10pt nocopyright onecolumn noqcourier notimes)

(define sigplan-extras
  (let ([abs (lambda (s)
               (path->collects-relative
                (collection-file-path s "scribble" "sigplan")))])
    (list
     (make-css-addition (abs "sigplan.css"))
     (make-tex-addition (abs "sigplan.tex")))))

;; ----------------------------------------
;; Abstracts:

(define abstract-style (make-style "abstract" sigplan-extras))

(define (abstract . strs)
  (make-nested-flow
   abstract-style
   (decode-flow strs)))

(define (extract-abstract p)
  (unless (part? p)
    (error 'include-abstract "doc binding is not a part: ~e" p))
  (unless (null? (part-parts p))
    (error 'include-abstract "abstract part has sub-parts: ~e" (part-parts p)))
  (when (part-title-content p)
    (error 'include-abstract "abstract part has title content: ~e" (part-title-content p)))
  (part-blocks p))

(define-syntax-rule (include-abstract mp)
  (begin
    (require (only-in mp [doc abstract-doc]))
    (make-nested-flow abstract-style (extract-abstract abstract-doc))))

;; ----------------------------------------
;; Authors and conference info:

(define (authorinfo name affiliation e-mail)
  ;; The \SAuthor macro in "style.tex" looks specifically
  ;; for an \SAuthorinfo as its argument, and handles it
  ;; specially in that case:
  (author
   (make-multiarg-element
    (make-style "SAuthorinfo" sigplan-extras)
    (list
     (make-element #f (decode-content (list name)))
     (make-element (make-style "SAuthorPlace" sigplan-extras)
                   (decode-content (list affiliation)))
     (make-element (make-style "SAuthorEmail" sigplan-extras)
                   (decode-content (list e-mail)))))))

(define (subtitle . str)
  (make-element (make-style "SSubtitle" (append '(aux) sigplan-extras))
                (decode-content str)))

(define (conferenceinfo what where)
  (make-paragraph
   (make-style 'pretitle null)
   (make-multiarg-element
    (make-style "SConferenceInfo" sigplan-extras)
    (list
     (make-element #f (decode-content (list what)))
     (make-element #f (decode-content (list where)))))))

(define (copyrightyear . when)
  (make-paragraph
   (make-style 'pretitle null)
   (make-element
    (make-style "SCopyrightYear" sigplan-extras)
    (decode-content when))))

(define (copyrightdata . what)
  (make-paragraph
   (make-style 'pretitle null)
   (make-element
    (make-style "SCopyrightData" sigplan-extras)
    (decode-content what))))

(define (doi . what)
  (make-paragraph
   (make-style 'pretitle null)
   (make-element
    (make-style "Sdoi" sigplan-extras)
    (decode-content what))))

(define (exclusive-license . what)
  (make-paragraph
   (make-style 'pretitle null)
   (make-element
    (make-style "SPexclusivelicense" sigplan-extras)
    (decode-content what))))

(define (to-appear . what)
  (make-paragraph
   (make-style 'pretitle null)
   (make-element
    (make-style "toappear" sigplan-extras)
    (decode-content what))))

;; ----------------------------------------
;; Categories, terms, and keywords:

(define (category sec title sub [more #f])
  (make-multiarg-element
   (make-style (format "SCategory~a" (if more "Plus" "")) sigplan-extras)
   (list*
    (make-element #f (decode-content (list sec)))
    (make-element #f (decode-content (list title)))
    (make-element #f (decode-content (list sub)))
    (if more
        (list (make-element #f (decode-content (list more))))
        null))))

(define (terms . str)
  (make-element (make-style "STerms" sigplan-extras) (decode-content str)))

(define (keywords . str)
  (make-element (make-style "SKeywords" sigplan-extras) (decode-content str)))

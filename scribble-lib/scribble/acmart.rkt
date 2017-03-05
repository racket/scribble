#lang racket/base
(require setup/collects
         racket/contract/base
         scribble/core
         scribble/base
         scribble/decode
         scribble/html-properties
         scribble/latex-properties
         (for-syntax racket/base))

(provide/contract
 [abstract 
  (->* () () #:rest (listof pre-content?)
       block?)]
 [acmJournal 
  (->* () () #:rest (listof pre-content?)
       content?)]
 [acmConference 
  (-> string? string? string? content?)]
 [grantsponsor 
  (-> string? string? string? content?)]
 [grantnum 
  (-> string? string? content?)]
 [acmBadgeR (-> string? content?)]
 [acmBadgeL (-> string? content?)]
 [citestyle (-> content? content?)]
 [CCSXML 
  (->* () () #:rest (listof pre-content?)
       any/c)])

(provide maketitle)

(define-syntax-rule (defopts name ...)
  (begin (define-syntax (name stx)
           (raise-syntax-error #f
                               "option must appear on the same line as `#lang scribble/acmart'"
                               stx))
         ...
         (provide name ...)))

(define-syntax-rule (define-commands name ...)
  (begin
    (begin
      (provide/contract [name (->* () () #:rest (listof pre-content?)
                                   content?)])
      (define (name . str)
        (make-element (make-style (symbol->string 'name) '(command))
                      (decode-content str))))
    ...))

(define-syntax-rule (define-environments name ...)
  (begin
    (begin
      (provide/contract [name (->* () () #:rest (listof pre-content?)
                                   block?)])
      (define (name . str)
        (make-nested-flow (make-style (symbol->string 'name) '())
                          (decode-flow str))))
    ...))

; format options
(defopts manuscript acmsmall acmlarge acmtog sigconf siggraph sigplan sigchi sigchi-a)
; boolean options
(defopts review screen natbib anonymous authorversion)

(define acmart-extras
  (let ([abs (lambda (s)
               (path->collects-relative
                (collection-file-path s "scribble" "acmart")))])
    (list
     (make-css-addition (abs "acmart.css"))
     (make-tex-addition (abs "acmart.tex")))))

;; ----------------------------------------
;; Abstracts:

(define abstract-style (make-style "abstract" acmart-extras))

(define (abstract . strs)
  (make-nested-flow
   abstract-style
   (decode-flow strs)))

(define (maketitle)
  (make-nested-flow (make-style "maketitle" '(command)) '()))

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


(define (acmJournal . str)
  (make-element (make-style "acmJournal" '(command))
                (decode-content str)))

(define (acmConference name date venue)
  (make-multiarg-element (make-style "acmConference" '(multicommand))
                         (list (decode-string name)
                               (decode-string date)
                               (decode-string venue))))

(define (grantsponsor id name url)
  (make-multiarg-element (make-style "grantsponsor" '(multicommand))
                         (list (decode-string id)
                               (decode-string name)
                               (decode-string url))))

(define (grantnum id num) ; FIXME: opt url
  (make-multiarg-element (make-style "grantnum" '(multicommand))
                         (list (decode-string id)
                               (decode-string num))))

;; FIXME: add support URL optional argument
(define (acmBadgeR str)
  (make-element (make-style "acmBadgeR" '(command))
                (decode-string str)))

(define (acmBadgeL str)
  (make-element (make-style "acmBadgeL" '(command))
                (decode-string str)))

(define (citestyle str)
  (make-element (make-style "citestyle" '(command))
                (decode-string str)))

(define-commands title subtitle orcid author affiliation email
  position institution department streetaddress city state postcode country
  thanks titlenote subtitlenote authornote acmVolume acmNumber acmArticle acmYear acmMonth
  acmArticleSeq acmPrice acmISBN acmDOI
  startPage terms keywords
  ccsdesc ; FIXME: add support for number opt arg
  setcopyright copyrightyear
  settopmatter ; could be "Rackety"
  received ; FIXME: opt stage
  shortauthors
  setcitstyle)

(define (CCSXML . str) ; FIXME: doesn't actual do exact-chars
  (make-nested-flow (make-style "CCSXML" '(exact-chars))
                    (decode-flow str)))

(define-environments teaserfigure sidebar marginfigure margintable
  printonly screenonly anonsuppress
  acks)

; FIXME: theorem styles


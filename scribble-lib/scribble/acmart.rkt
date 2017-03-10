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
 [title (->* (pre-content?) (#:short pre-content?) content?)]
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
  (->* (string? string?) (#:url string?) content?)]
 [acmBadgeR (->* (string?) (#:url string?) content?)]
 [acmBadgeL (->* (string?) (#:url string?) content?)]
 [received (->* (string?) (#:stage string?) content?)]
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
        (make-element (make-style (symbol->string 'name) command-props)
                      (decode-content str))))
    ...))

(define-syntax-rule (define-environments name ...)
  (begin
    (begin
      (provide/contract [name (->* () () #:rest (listof pre-content?)
                                   block?)])
      (define (name . str)
        (make-nested-flow (make-style (symbol->string 'name) acmart-extras)
                          (decode-flow str))))
    ...))

; comment environments ensure the \begin and \end are on their own lines
(define-syntax-rule (define-comment-environments name ...)
  (begin
    (begin
      (provide/contract [name (->* () () #:rest (listof pre-content?)
                                   block?)])
      (define (name . str)
        (make-nested-flow (make-style (symbol->string 'name) acmart-extras)
                          (append (list (make-paragraph (style #f '()) '("")))
                                  (decode-flow str)
                                  (list (make-paragraph (style #f '()) '("")))))))
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

(define command-props (cons 'command acmart-extras))
(define multicommand-props (cons 'multicommand acmart-extras))

(define (abstract . strs)
  (make-nested-flow
   abstract-style
   (decode-flow strs)))

(define (maketitle)
  (make-nested-flow (make-style "maketitle" command-props) '()))

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
  (make-element (make-style "acmJournal" command-props)
                (decode-content str)))

(define (acmConference name date venue)
  (make-multiarg-element (make-style "acmConference" multicommand-props)
                         (list (decode-string name)
                               (decode-string date)
                               (decode-string venue))))

(define (grantsponsor id name url)
  (make-multiarg-element (make-style "grantsponsor" multicommand-props)
                         (list (decode-string id)
                               (decode-string name)
                               (decode-string url))))

(define (grantnum #:url [url #f] id num)
  (if url
      (make-multiarg-element (make-style "SgrantnumURL" multicommand-props)
                             (list (decode-string url)
                                   (decode-string id)
                                   (decode-string num)))
      (make-multiarg-element (make-style "grantnum" multicommand-props)
                             (list (decode-string id)
                                   (decode-string num)))))

(define (acmBadgeR #:url [url #f] str)
  (if url
      (make-multiarg-element (make-style "SacmBadgeRURL" multicommand-props)
                             (list (decode-string url)
                                   (decode-string str)))
      (make-element (make-style "acmBadgeR" command-props)
                    (decode-string str))))

(define (acmBadgeL #:url [url #f] str)
  (if url
      (make-multiarg-element (make-style "SacmBadgeLURL" multicommand-props)
                             (list (decode-string url)
                                   (decode-string str)))
      (make-element (make-style "acmBadgeL" command-props)
                    (decode-string str))))

(define (received #:stage [s #f] str)
  (if s
      (make-multiarg-element (make-style "SreceivedStage" multicommand-props)
                             (list (decode-string s)
                                   (decode-string str)))
      (make-element (make-style "received" command-props)
                    (decode-string str))))

(define (citestyle str)
  (make-element (make-style "citestyle" command-props)
                (decode-string str)))

(define (ccsdesc #:number [n #f] str)
  (if n
      (make-multiarg-element (make-style "SccsdescNumber" multicommand-props)
                             (list (number->string n)
                                   (decode-string str)))
      (make-element (make-style "ccsdesc" command-props)
                    (decode-string str))))

(define (title #:short [st #f] str)
  (if st
      (make-multiarg-element (make-style "StitleShort" multicommand-props)
                             (list (decode-string st)
                                   (decode-string str)))
      (make-element (make-style "title" command-props)
                    (decode-string str))))
      

(define-commands subtitle orcid author affiliation email
  position institution department streetaddress city state postcode country
  thanks titlenote subtitlenote authornote acmVolume acmNumber acmArticle acmYear acmMonth
  acmArticleSeq acmPrice acmISBN acmDOI
  startPage terms keywords
  setcopyright copyrightyear
  settopmatter ; could be "Rackety"
  shortauthors
  setcitstyle)

(define (CCSXML . strs)
  (make-nested-flow (make-style "CCSXML" '())
                    (list (make-paragraph (make-style #f '())
                                          (make-element (make-style #f '(exact-chars))
                                                        (apply string-append strs))))))

(define-environments teaserfigure sidebar marginfigure margintable)
(define-comment-environments printonly screenonly anonsuppress acks)

; FIXME: theorem styles


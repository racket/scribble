#lang racket/base

(require setup/collects
         racket/contract/base
         racket/list
         scribble/core
         scribble/base
         scribble/decode
         scribble/html-properties
         scribble/latex-properties
         scribble/private/tag
         (for-syntax racket/base))

(struct affiliation (position institution department street-address city state postcode country)
  #:constructor-name author-affiliation
  #:name author-affiliation
  #:transparent)

(provide/contract
 [title (->* ()
             (#:short pre-content?
              #:tag (or/c string? (listof string?) #f)
              #:tag-prefix (or/c string? module-path? #f)
              #:style (or/c style? string? symbol? #f)
              #:version (or/c string? #f)
              #:date (or/c string? #f))
             #:rest (listof pre-content?)
             title-decl?)]
 [author (->* ()
              (#:orcid (or/c string? #f)
               #:affiliation (or/c pre-content?
                                   affiliation?
                                   (listof pre-content?)
                                   (listof affiliation?)
                                   #f)
               #:email (or/c pre-content? (listof pre-content?) #f))
              #:rest (listof pre-content?)
              block?)]
 [affiliation (->* ()
                   (#:position (or/c string? #f)
                    #:institution (or/c string? #f)
                    #:department (or/c string? #f)
                    #:street-address (or/c string? #f)
                    #:city (or/c string? #f)
                    #:state (or/c string? #f)
                    #:postcode (or/c string? #f)
                    #:country (or/c string? #f))
                   affiliation?)]
 [affiliation? (-> any/c boolean?)]
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
(defopts review screen natbib anonymous authorversion 9pt 10pt 11pt 12pt)

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

(define (title #:tag [tag #f]
               #:tag-prefix [prefix #f]
               #:style [style plain]
               #:version [version #f]
               #:date [date #f]
               #:short [short #f]
               . str)
  (let ([content (decode-content str)])
    (make-title-decl (prefix->string prefix)
                     (convert-tag tag content)
                     version
                     (let* ([s (convert-part-style 'title style)]
                            [s (if date
                                   (make-style (style-name s)
                                               (cons (make-document-date date)
                                                     (style-properties s)))
                                   s)]
                            [s (if short
                                   (make-style (style-name s)
                                               (cons (short-title short)
                                                     (style-properties s)))
                                   s)])
                       s)
                     content)))

(define (author #:orcid [orcid #f]
                #:affiliation [affiliation '()]
                #:email [email '()]
                . name)
  (make-paragraph
   (make-style 'author command-props)
   (decode-content
    (make-multiarg-element (make-style "SAuthorinfo" multicommand-props)
                           (list (make-element #f (decode-content name))
                                 (make-element #f
                                               (if orcid
                                                   (make-element
                                                    (make-style "SAuthorOrcid" multicommand-props)
                                                    (decode-content (list orcid)))
                                                   '()))
                                 (make-element #f
                                               (for/list ([a (in-list (if (pair? affiliation)
                                                                          affiliation
                                                                          (list affiliation)))])
                                                 (if (affiliation? a)
                                                     (convert-affiliation a)
                                                     (make-element
                                                      (make-style "SAuthorPlace" multicommand-props)
                                                      (decode-content (list a))))))
                                 (make-element #f
                                               (for/list ([e (in-list (if (pair? email)
                                                                          email
                                                                          (list email)))])
                                                 (make-element
                                                  (make-style "SAuthorEmail" multicommand-props)
                                                  (decode-content (list e))))))))))
  
(define (affiliation #:position [position #f]
                     #:institution [institution #f]
                     #:department [department #f]
                     #:street-address [street-address #f]
                     #:city [city #f]
                     #:state [state #f]
                     #:postcode [postcode #f]
                     #:country [country #f])
  (author-affiliation position institution department street-address city state postcode country))

(define (convert-affiliation aff)
  (define (maybe-element str content)
    (and (content aff) (make-element str (decode-content (list (content aff))))))
  (make-multiarg-element
   (make-style "SAuthorPlace" multicommand-props)
   (filter values
           (list (maybe-element "position" affiliation-position)
                 (maybe-element "institution" affiliation-institution)
                 (maybe-element "department" affiliation-department)
                 (maybe-element "streetaddress" affiliation-street-address)
                 (maybe-element "city" affiliation-city)
                 (maybe-element "state" affiliation-state)
                 (maybe-element "postcode" affiliation-postcode)
                 (maybe-element "country" affiliation-country))))) 
(define-commands subtitle
  thanks titlenote subtitlenote authornote acmVolume acmNumber acmArticle acmYear acmMonth
  acmArticleSeq acmPrice acmISBN acmDOI
  startPage terms keywords
  setcopyright copyrightyear
  settopmatter ; could be "Rackety"
  shortauthors)

(define (CCSXML . strs)
  (make-nested-flow (make-style "CCSXML" '())
                    (list (make-paragraph (make-style #f '())
                                          (make-element (make-style #f '(exact-chars))
                                                        (apply string-append strs))))))

(define-environments teaserfigure sidebar marginfigure margintable)
(define-comment-environments printonly screenonly anonsuppress acks)

; FIXME: theorem styles


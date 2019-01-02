#lang racket/base

(require setup/collects
         racket/contract/base
         racket/list
         racket/string
         scribble/core
         scribble/base
         scribble/decode
         scribble/html-properties
         scribble/latex-properties
         scribble/private/tag
         scribble/private/lang-parameters
         (for-syntax racket/base))

(struct affiliation (position institution street-address city state postcode country)
  #:constructor-name author-affiliation
  #:name author-affiliation
  #:transparent)

(struct email (text)
  #:constructor-name author-email
  #:name author-email
  #:transparent)

(struct institution (name departments)
  #:constructor-name author-institution
  #:name author-institution
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
              (#:orcid (or/c pre-content? #f)
               #:affiliation (or/c pre-content?
                                   affiliation?
                                   (listof affiliation?)
                                   #f)
               #:email (or/c pre-content? email? (listof email?)))
              #:rest (listof pre-content?)
              block?)]
 [authorsaddresses (->* ()
                        ()
                        #:rest (listof pre-content?)
                        block?)]
 [shortauthors (->* ()
                    ()
                    #:rest (listof pre-content?)
                    element?)]
 [institution (->* ()
                   (#:departments (listof (or/c pre-content? institution?)))
                   #:rest pre-content?
                   institution?)]
 [institution? (-> any/c boolean?)]
 [email (->* ()
             #:rest (listof pre-content?)
             email?)]
 [email-string (->* ()
                    #:rest (listof string?)
                    email?)]
 [email? (-> any/c boolean?)]
 [affiliation (->* ()
                   (#:position (or/c pre-content? #f)
                    #:institution (or/c pre-content? institution? (listof institution?) #f)
                    #:street-address (or/c pre-content? #f)
                    #:city (or/c pre-content? #f)
                    #:state (or/c pre-content? #f)
                    #:postcode (or/c pre-content? #f)
                    #:country (or/c pre-content? #f))
                   affiliation?)]
 [affiliation? (-> any/c boolean?)]
 [abstract 
  (->* () () #:rest (listof pre-content?)
       block?)]
 [acmConference 
  (-> string? string? string? block?)]
 [grantsponsor 
  (-> string? string? string? content?)]
 [grantnum 
  (->* (string? string?) (#:url string?) content?)]
 [acmBadgeR (->* (string?) (#:url string?) block?)]
 [acmBadgeL (->* (string?) (#:url string?) block?)]
 [received (->* (string?) (#:stage string?) block?)]
 [citestyle (-> content? block?)]
 [ccsdesc (->* (string?) (#:number exact-integer?) block?)]
 [CCSXML 
  (->* () () #:rest (listof pre-content?)
       any/c)])
(provide
  invisible-element-to-collect-for-acmart-extras
  include-abstract)

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
                                   block?)])
      (define (name . str)
        (make-paragraph (make-style 'pretitle '())
                        (make-element (make-style (symbol->string 'name) command-props)
                                      (decode-content str)))))
    ...))

(define-syntax-rule (define-environments name ...)
  (begin
    (begin
      (provide/contract [name (->* () () #:rest (listof pre-flow?)
                                   block?)])
      (define (name . str)
        (make-nested-flow (make-style (symbol->string 'name) acmart-extras)
                          (decode-flow str))))
    ...))

; comment environments ensure the \begin and \end are on their own lines
(define-syntax-rule (define-comment-environments name ...)
  (begin
    (begin
      (provide/contract [name (->* () () #:rest (listof pre-flow?)
                                   block?)])
      (define (name . str)
        (make-nested-flow (make-style (symbol->string 'name) acmart-extras)
                          (append (list (make-paragraph (style #f '()) '("")))
                                  (decode-flow str)
                                  (list (make-paragraph (style #f '()) '("")))))))
    ...))

; format options
(defopts manuscript acmsmall acmlarge acmtog sigconf siggraph sigplan sigchi sigchi-a
  dtrap pacmcgit tiot tdsci)
; boolean options
(defopts review screen natbib anonymous authorversion 9pt 10pt 11pt 12pt)

(define acmart-extras
  (let ([abs (lambda (s)
               (path->collects-relative
                (collection-file-path s "scribble" "acmart")))])
    (list
     (make-css-addition (abs "acmart.css"))
     (make-tex-addition (abs "acmart.tex")))))

(define invisible-element-to-collect-for-acmart-extras
  (make-element (make-style "invisible-element-to-collect-for-acmart-extras" acmart-extras) '()))

;; ----------------------------------------
;; Abstracts:

(define abstract-style (make-style "abstract" (cons 'pretitle acmart-extras)))

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

(define (acmConference name date venue)
  (make-paragraph (make-style 'pretitle '())
                  (make-multiarg-element (make-style "acmConference" multicommand-props)
                                         (list (decode-string name)
                                               (decode-string date)
                                               (decode-string venue)))))

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
  (make-paragraph (make-style 'pretitle '())
                  (if url
                      (make-multiarg-element (make-style "SacmBadgeRURL" (cons 'exact-chars multicommand-props))
                                             (list (decode-string url)
                                                   (decode-string str)))
                      (make-element (make-style "acmBadgeR" (cons 'exact-chars command-props))
                                    (decode-string str)))))
  
(define (acmBadgeL #:url [url #f] str)
  (make-paragraph (make-style 'pretitle '())
                  (if url
                      (make-multiarg-element (make-style "SacmBadgeLURL" (cons 'exact-chars multicommand-props))
                                             (list (decode-string url)
                                                   (decode-string str)))
                      (make-element (make-style "acmBadgeL" (cons 'exact-chars command-props))
                                    (decode-string str)))))

(define (received #:stage [s #f] str)
  (make-paragraph (make-style 'pretitle '())
                  (if s
                      (make-multiarg-element (make-style "SreceivedStage" multicommand-props)
                                             (list (decode-string s)
                                                   (decode-string str)))
                      (make-element (make-style "received" command-props)
                                    (decode-string str)))))

(define (citestyle str)
  (make-paragraph (make-style 'pretitle '())
                  (make-element (make-style "citestyle" command-props)
                                (decode-string str))))

(define (ccsdesc #:number [n #f] str)
  (make-paragraph (make-style 'pretitle '())
                  (if n
                      (make-multiarg-element (make-style "SccsdescNumber" (cons 'exact-chars multicommand-props))
                                             (list (number->string n)
                                                   (decode-string str)))
                      (make-element (make-style "ccsdesc" (cons 'exact-chars command-props))
                                    (decode-string str)))))

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
    (list
     (make-multiarg-element (make-style "SAuthorinfo" multicommand-props)
                            (list (make-element #f (decode-content name))
                                  (make-element #f
                                                (if orcid
                                                    (make-element
                                                     (make-style "SAuthorOrcid" multicommand-props)
                                                     (decode-content (list orcid)))
                                                    '()))
                                  (make-element #f
                                                (cond
                                                  [(affiliation? affiliation)
                                                   (convert-affiliation affiliation)]
                                                  [(pre-content? affiliation)
                                                   (make-element
                                                    (make-style "SAuthorPlace" multicommand-props)
                                                    (decode-content (list affiliation)))]
                                                  [else
                                                   (for/list ([a (in-list affiliation)])
                                                     (convert-affiliation a))]))
                                  (make-element #f
                                                (cond
                                                  [(email? email)
                                                   (convert-email email)]
                                                  [(pre-content? email)
                                                   (make-element
                                                    (make-style "SAuthorEmail" multicommand-props)
                                                    (decode-content (list email)))]
                                                  [else
                                                   (for/list ([e (in-list email)])
                                                     (convert-email e))]))))))))

(define (authorsaddresses . content)
  (make-paragraph
   (make-style 'pretitle command-props)
   (make-element (make-style "authorsaddresses" command-props)
                 (decode-content content))))

(define (shortauthors . content)
  (make-element (make-style "Sshortauthors" command-props)
                (decode-content content)))

(define (institution #:departments [departments '()]
                     . name)
  (author-institution name departments))

(define (convert-institution inst
                             #:department? [department? #f])
  (define level 0)
  (define (mk-inst name
                   #:department? [department? department?]
                   #:level [level level])
    (case department?
      [(#f) (make-element (make-style "institution" command-props)
                          (decode-content name))]
      [(sub) (make-element (make-style "department"
                                       (cons (command-optional (list (number->string level)))
                                             command-props))
                           (decode-content name))]
      [else (make-element (make-style "department"
                                      (append
                                       (if (> level 0)
                                           (list (command-optional (list (number->string level))))
                                           (list))
                                       command-props))
                          (decode-content name))]))
  (define lst
    (append
     (for/list ([i (in-list (institution-departments inst))])
       (cond
         [(institution? i)
          (define-values (content new-level)
            (convert-institution i
                                 #:department? (or (and department? 'sub)
                                                   #t)))
          (set! level (max level (+ 1 new-level)))
          content]
         [else
          (set! level 1)
          (mk-inst (list i)
                   #:department? (or (and department? 'sub)
                                     #t)
                   #:level 0)]))
     (list (mk-inst (institution-name inst)))))
  (if department?
      (values lst level)
      lst))

(define (email . text)
  (author-email text))

(define (email-string . text)
  (define text-escaped
    (for/list ([str (in-list text)])
      (escape-email-string str)))
  (author-email
   (list
    (make-element
     (make-style #f '(exact-chars))
     text-escaped))))

(define (convert-email email)
  (make-element
   (make-style "SAuthorEmail" command-props)
   (decode-content (email-text email))))

(define escape-email-map
  #(("#" . "\\#")
    ("%" . "\\%")))
(define (escape-email-string str)
  (for/fold ([str str])
            ([escape-map (in-vector escape-email-map)])
    (string-replace str (car escape-map) (cdr escape-map))))

(define (affiliation #:position [position #f]
                     #:institution [institution #f]
                     #:street-address [street-address #f]
                     #:city [city #f]
                     #:state [state #f]
                     #:postcode [postcode #f]
                     #:country [country #f])
  (author-affiliation position institution street-address city state postcode country))

(define (convert-affiliation aff)
  (define (maybe-element str content)
    (and (content aff) (make-element str (decode-content (list (content aff))))))
  (make-element
   (make-style "SAuthorPlace" command-props)
   (make-multiarg-element
    (make-style #f multicommand-props)
    (filter values
            (append (list (maybe-element "position" affiliation-position))
                    (if (institution? (affiliation-institution aff))
                        (convert-institution (affiliation-institution aff))
                        (list (maybe-element "institution" affiliation-institution)))
                    (list (maybe-element "streetaddress" affiliation-street-address)
                          (maybe-element "city" affiliation-city)
                          (maybe-element "state" affiliation-state)
                          (maybe-element "postcode" affiliation-postcode)
                          (maybe-element "country" affiliation-country)))))))
  
(define-commands subtitle acmJournal
  thanks titlenote subtitlenote authornote acmVolume acmNumber acmArticle acmYear acmMonth
  acmArticleSeq acmPrice acmISBN acmDOI
  startPage terms keywords
  setcopyright copyrightyear
  settopmatter hortauthors)

(define (CCSXML . strs)
  (make-nested-flow (make-style "CCSXML" '())
                    (list (make-paragraph (make-style #f '())
                                          (make-element (make-style #f '(exact-chars))
                                                        (apply string-append strs))))))

(define-environments teaserfigure sidebar marginfigure margintable)
(define-comment-environments printonly screenonly anonsuppress acks)

; FIXME: theorem styles

(default-figure-label-text (make-element 'sf "Fig."))
(default-figure-label-sep ". ")
(default-figure-caption-style 'sf)
(default-figure-counter-style 'sf)

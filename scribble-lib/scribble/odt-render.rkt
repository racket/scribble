#lang racket

;; Notes from Christopher Lemmer Weber, Oct 15 2020:
;; =================================================
;;
;; A lot of this was done in a rush.  What I did was a hodgepodge of:
;;
;;  - Look at org-mode's odt exporter.  Honestly probably the best
;;    readable documentation that you can find.  If I looked at it
;;    more this probably would be better. ;)
;;  - Did an export from org-mode to ODT and a file saved from Libreoffice.
;;    Poked around at both.
;;  - Occasionally checked the ODT spec.
;;  - Tried to read the Scribble html renderer, went a bit mad from
;;    too much information, scaled back and read the Scribble docs more
;;    carefully, then tried reading the Markdown renderer (much easier!)
;;
;; Probably this could be a lot better but hey, it's a start, right...?

(provide render-mixin)

(require "core.rkt"
         file/convertible
         (prefix-in xml: xml/xml)

         file/zip
         racket/draw
         [only-in pict
                  pict?
                  pict-width pict-height])

(define xml-header
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")

;; TODO: We're not using all of these, so some of the namespaces here
;; could be stripped... or commented out to be possibly included later.
(define odt-namespaces
  '((office . "urn:oasis:names:tc:opendocument:xmlns:office:1.0")
    (style . "urn:oasis:names:tc:opendocument:xmlns:style:1.0")
    (text . "urn:oasis:names:tc:opendocument:xmlns:text:1.0")
    (table . "urn:oasis:names:tc:opendocument:xmlns:table:1.0")
    (draw . "urn:oasis:names:tc:opendocument:xmlns:drawing:1.0")
    (fo . "urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0")
    (xlink . "http://www.w3.org/1999/xlink")
    (dc . "http://purl.org/dc/elements/1.1/")
    (meta . "urn:oasis:names:tc:opendocument:xmlns:meta:1.0")
    (number . "urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0")
    (svg . "urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0")
    (chart . "urn:oasis:names:tc:opendocument:xmlns:chart:1.0")
    (dr3d . "urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0")
    (math . "http://www.w3.org/1998/Math/MathML")
    (form . "urn:oasis:names:tc:opendocument:xmlns:form:1.0")
    (script . "urn:oasis:names:tc:opendocument:xmlns:script:1.0")
    (ooo . "http://openoffice.org/2004/office")
    (ooow . "http://openoffice.org/2004/writer")
    (oooc . "http://openoffice.org/2004/calc")
    (dom . "http://www.w3.org/2001/xml-events")
    (xforms . "http://www.w3.org/2002/xforms")
    (xsd . "http://www.w3.org/2001/XMLSchema")
    (xsi . "http://www.w3.org/2001/XMLSchema-instance")
    (rpt . "http://openoffice.org/2005/report")
    (of . "urn:oasis:names:tc:opendocument:xmlns:of:1.2")
    (xhtml . "http://www.w3.org/1999/xhtml")
    (grddl . "http://www.w3.org/2003/g/data-view#")
    (officeooo . "http://openoffice.org/2009/office")
    (tableooo . "http://openoffice.org/2009/table")
    (drawooo . "http://openoffice.org/2010/draw")
    (calcext . "urn:org:documentfoundation:names:experimental:calc:xmlns:calcext:1.0")
    (loext . "urn:org:documentfoundation:names:experimental:office:xmlns:loext:1.0")
    (field . "urn:openoffice:names:experimental:ooo-ms-interop:xmlns:field:1.0")
    (formx . "urn:openoffice:names:experimental:ooxml-odf-interop:xmlns:form:1.0")
    (css3t . "http://www.w3.org/TR/css3-text/")))

(define xmlns-props
  (map (match-lambda
         [(cons key val)
          (list (string->symbol (format "xmlns:~a" key))
                val)])
       odt-namespaces))

(define default-font-decls
  `(office:font-face-decls
    ,xmlns-props
    (style:font-face
     ((svg:font-family "'Liberation Serif'")
      (style:name "Liberation Serif")
      (style:font-pitch "variable")
      (style:font-family-generic "roman")))
    (style:font-face
     ((svg:font-family "'Liberation Sans'")
      (style:name "Liberation Sans")
      (style:font-pitch "variable")
      (style:font-family-generic "swiss")))))

(define default-styles
  `(office:document-styles
    ((office:version "1.2")
     ,@xmlns-props)
    (office:font-face-decls
     (style:font-face
      ((svg:font-family "'Liberation Serif'")
       (style:name "Liberation Serif")
       (style:font-pitch "variable")
       (style:font-family-generic "roman")))
     (style:font-face
      ((svg:font-family "'Liberation Sans'")
       (style:name "Liberation Sans")
       (style:font-pitch "variable")
       (style:font-family-generic "swiss"))))
    ;; This section probably has some stuff that could be removed...
    (office:styles
     (style:default-style
      ((style:family "paragraph"))
      (style:paragraph-properties
       ((style:writing-mode "page")
        (style:text-autospace "ideograph-alpha")
        (style:tab-stop-distance "0.4925in")
        (style:punctuation-wrap "hanging")
        (style:line-break "strict")
        (fo:widows "2")
        (fo:orphans "2")
        (fo:hyphenation-ladder-count "no-limit")))
      (style:text-properties
       ((style:use-window-font-color "true")
        (style:letter-kerning "true")
        (style:language-complex "hi")
        (style:language-asian "zh")
        (style:font-size-complex "12pt")
        (style:font-size-asian "10.5pt")
        (style:font-name-complex "FreeSans")
        (style:font-name-asian "Noto Sans CJK SC DemiLight")
        (style:font-name "Liberation Serif")
        (style:country-complex "IN")
        (style:country-asian "CN")
        (fo:language "en")
        (fo:hyphenation-remain-char-count "2")
        (fo:hyphenation-push-char-count "2")
        (fo:hyphenate "false")
        (fo:font-size "12pt")
        (fo:country "US"))))

     (style:default-style
      ((style:family "table"))
      (style:table-properties ((table:border-model "collapsing"))))
     (style:default-style
      ((style:family "table-row"))
      (style:table-row-properties ((fo:keep-together "auto"))))

     ;; Start intentional styles
     (style:style ((style:name "Bold")
                   (style:family "text"))
                  (style:text-properties ((fo:font-weight "bold"))))
     ;; Maybe differentiate this to switch in and out of italics
     (style:style ((style:name "Emphasis")
                   (style:family "text"))
                  (style:text-properties ((fo:font-style "italic"))))
     (style:style ((style:name "Italic")
                   (style:family "text"))
                  (style:text-properties ((fo:font-style "italic"))))

     (style:style ((style:name "MainContent")
                   (style:family "paragraph")
                   (style:class "text")
                   (style:display-name "Main Content"))
                  (style:paragraph-properties ((fo:line-height "200%")
                                               (fo:text-indent "0.5in"))))

     (style:style ((style:name "Centered")
                   (style:family "paragraph")
                   (style:class "text")
                   (style:display-name "Centered"))
                  (style:paragraph-properties ((fo:line-height "200%")
                                               (fo:text-align "center"))))

     (style:style ((style:name "Author")
                   (style:family "paragraph")
                   (style:class "text")
                   (style:display-name "Author"))
                  (style:paragraph-properties ((fo:text-align "center")))
                  (style:text-properties ((fo:font-style "italic")
                                          (fo:font-size "150%"))))

     (style:style
      ((style:name "BiblioEntry")
       (style:family "paragraph")
       (style:class "text"))
      (style:paragraph-properties ((fo:text-indent "-0.5in")
                                   (fo:margin-left "0.5in")
                                   (fo:margin-bottom "0.2in"))))

     (style:style ((style:name "Chapter_Heading")
                   (style:class "text")
                   (style:family "paragraph"))
                  (style:paragraph-properties ((fo:break-before "page")
                                               (fo:text-align "center")
                                               (fo:margin-top "1cm")
                                               (fo:margin-bottom "1cm")
                                               (fo:keep-with-next "always")))
                  (style:text-properties ((fo:font-size "24pt")
                                          (fo:font-weight "bold"))))
     (style:style ((style:name "Heading_1")
                   (style:class "text")
                   (style:family "paragraph"))
                  (style:paragraph-properties ((fo:margin-top "0.35cm")
                                               (fo:margin-bottom "0.35cm")
                                               (fo:keep-with-next "always")))
                  (style:text-properties ((fo:font-size "18pt")
                                          (fo:font-weight "bold"))))
     (style:style ((style:name "Heading_2")
                   (style:class "text")
                   (style:family "paragraph"))
                  (style:paragraph-properties ((fo:margin-top "0.35cm")
                                               (fo:margin-bottom "0.35cm")
                                               (fo:keep-with-next "always")))
                  (style:text-properties ((fo:font-size "16pt")
                                          (fo:font-weight "bold"))))
     (style:style ((style:name "Heading_3")
                   (style:class "text")
                   (style:family "paragraph"))
                  (style:paragraph-properties ((fo:margin-top "0.35cm")
                                               (fo:margin-bottom "0.35cm")
                                               (fo:keep-with-next "always")))
                  (style:text-properties ((fo:font-size "14pt")
                                          (fo:font-weight "bold")))))))

(define default-sequence-decls
  '(text:sequence-decls
    (text:sequence-decl
     ((text:name "Illustration")
      (text:display-outline-level "0")))
    (text:sequence-decl
     ((text:name "Table")
      (text:display-outline-level "0")))
    (text:sequence-decl
     ((text:name "Text")
      (text:display-outline-level "0")))
    (text:sequence-decl
     ((text:name "Drawing")
      (text:display-outline-level "0")))))


(define in-quotation?
  (make-parameter #f))

(define in-centered?
  (make-parameter #f))

(define installed-images  ; mutable set when instantiated
  (make-parameter #f))

(define document-main-paragraphs?
  (make-parameter #t))

(define-syntax-rule (leave-main-para body ...)
  (parameterize ([document-main-paragraphs? #f])
    body ...))


(define (render-mixin %)
  (class %
    (inherit render-part
             format-number
             number-depth
             render-block
             report-output?)

    ;; dynamically instantiated
    (define tmp-dir
      (make-parameter #f))

    (define/override (current-render-mode)
      '(odt-content))
    (define/override (get-suffix) #".odt")

    (define/override (render-paragraph p part ri)
      (let* ([contents
              (leave-main-para
               (super render-paragraph p part ri))]
             [style (paragraph-style p)])
        (match (style-name style)
          ['author
           `(;; Right now uncommenting this adds a comment to every major
             ;; section or chapter, which doesn't seem right..
             #;(text:p ((text:style-name "Author")) ,@contents))]
          [_
           `((text:p ,@(cond
                         [(in-quotation?)
                          '(((text:style-name "Quotations")))]
                         [(in-centered?)
                          '(((text:style-name "Centered")))]
                         [(document-main-paragraphs?)
                          '(((text:style-name "MainContent")))]
                         [else '()])
                     ,@contents))])))

    ;; Predicates here largely taken from the markdown renderer...
    (define/private (content-style e)
      (cond
        [(element? e) (element-style e)]
        [(multiarg-element? e) (multiarg-element-style e)]
        [else #f]))

    (define (bold? i)
      (and (element? i) (eq? (element-style i) 'bold)))

    (define (italic? i)
      (and (element? i) (eq? (element-style i) 'italic)))

    (define (emphasis? i)
      (and (element? i) (eq? (element-style i) 'emph)))

    (define (link? i)
      (let ([s (content-style i)])
        (and (style? s) (findf target-url? (style-properties s)))))

    (define (link-from i)
      (target-url-addr (findf target-url? (style-properties (content-style i)))))

    (define (code? i)
      (and (element? i)
           (let ([s (element-style i)])
             (or (eq? 'tt s)
                 (and (style? s)
                      (style-name s)
                      (regexp-match? #rx"^Rkt[A-Z]" (style-name s)))))))

    (define (footnote? i)
      (match i
        [(element (style (or "NoteBox" "Footnote") _) _)
         #t]
        [_ #f]))

    (define (next-image-filename extension)
      (format "image~a.~a" (sequence-length (installed-images)) extension))

    (define (mimetype-extension-from-image-path image-path)
      (define path-str
        (path->string image-path))
      (cond
        [(regexp-match #rx"\\.png" path-str)
         (values "image/png" "png")]
        [(regexp-match #rx"\\.gif" path-str)
         (values "image/gif" "gif")]
        [(regexp-match #rx"\\.jpg" path-str)
         (values "image/jpeg" "jpg")]
        [else
         (error 'unknown-image-type "Unknown image type: ~a" path-str)]))

    ;; kinda horrible kluge that serves my needs at the moment
    (define (pixels-to-cm width height)
      (define max-cm 10)
      (define-values (cm-width cm-height)
        (if (> width height)
            (let ([ratio (/ height width)])
              (values max-cm (* ratio max-cm)))
            (let ([ratio (/ width height)])
              (values (* ratio max-cm) max-cm))))
      (define (~cm num)
        (string-append (~r num #:precision '(= 2))
                       "cm"))
      (values (~cm cm-width) (~cm cm-height)))

    (define (install-register-image image-data-or-path
                                    [mimetype "image/png"]
                                    [extension "png"]
                                    [width #f] [height #f])
      (define images-dir
        (build-path (tmp-dir) "Images"))
      (unless (directory-exists? images-dir)
        (make-directory images-dir))
      (define image-filename
        (next-image-filename extension))
      (define image-fullpath
        (build-path images-dir image-filename))

      (match image-data-or-path
        [(? bytes? orig-image-bytes)
         (call-with-output-file image-fullpath
           (lambda (op)
             (write-bytes orig-image-bytes op)))]
        [(? path? orig-image-path)
         (copy-file orig-image-path image-fullpath)])

      (set-add! (installed-images)
                (vector 'img image-filename mimetype))
      (format "Images/~a" image-filename))

    (define/override (render-content i part ri)
      (cond
        [(bold? i)
         `((text:span ((text:style-name "Bold"))
                      ,@(super render-content i part ri)))]
        [(italic? i)
         `((text:span ((text:style-name "Italic"))
                      ,@(super render-content i part ri)))]
        [(emphasis? i)
         `((text:span ((text:style-name "Emphasis"))
                      ,@(super render-content i part ri)))]
        [(footnote? i)
         (leave-main-para
          `((text:note ((text:note-class "footnote"))  ; TODO: text:id
                       (text:note-body
                        (text:p ,@(super render-content i part ri))))))]
        [(link? i)
         `((text:a ((xlink:type "simple")
                    (xlink:href ,(link-from i)))
                   ,@(apply append
                            (for/list ([item (element-content i)])
                              (render-content item part ri)))))]
        [(image-element? i)
         (define img-path (image-element-path i))
         (define img-bitmap (read-bitmap img-path))
         (define-values (cm-width cm-height)
           (pixels-to-cm (send img-bitmap get-width)
                         (send img-bitmap get-height)))

         ;; TODO: scaling, etc.
         `((draw:frame ((svg:width ,cm-width)
                        (svg:height ,cm-height))
                       (draw:image ((xlink:href
                                     ,(install-register-image (image-element-path i)))
                                    (xlink:type "simple")
                                    (xlink:show "embed")
                                    (xlink:actuate "onLoad")))))]

        [(convertible? i)
         ;; Let's try rendering
         (cond
           [(convert i 'png-bytes #f)
            => (lambda (converted)
                 (define-values (width height)
                   (cond
                     [(pict? i)
                      (pixels-to-cm (pict-width i) (pict-height i))]
                     [else
                      (values #f #f)]))
                 `((draw:frame ,(or (and width height
                                         `((svg:width ,width)
                                           (svg:height ,height)))
                                    '((svg:width "5cm")
                                      (svg:height "5cm")))
                               (draw:image ((xlink:href
                                             ,(install-register-image converted
                                                                      "image/png"))
                                            (xlink:type "simple")
                                            (xlink:show "embed")
                                            (xlink:actuate "onLoad"))))))]
           [else
            (super render-content i part ri)])]
        [else
         (super render-content i part ri)]))


    (define/override (render-part-content d ri)
      (define number
        (collected-info-number (part-collected-info d ri)))
      (define section-name
        (for/or ([t (in-list (part-tags d))])
          (match t
            [(list 'part tag-val)
             (if (generated-tag? tag-val)
                 (~s (tag-key t ri))
                 (~s tag-val))]
            [_ #f])))
      `((text:section
         ,@(if section-name
               `(([text:name ,section-name]))
               '())
         ,@(if (part-title-content d)
               ;; Well it doesn't seem to be paying attention to *my* heading
               ;; styles but it does work with whatever ...?
               `((text:h ((text:style-name ,(case (number-depth number)
                                              [(0) "Title"]
                                              [(1) "Chapter_Heading"]
                                              [(2) "Heading_1"]
                                              [else "Heading_2"])))
                         ,@(if (= (number-depth number) 1)
                               `("Chapter "
                                 ,@(format-number number '(" "))
                                 ": "
                                 ,@(render-content (part-title-content d) d ri))
                               `(,@(format-number number '(" "))
                                 ,@(render-content (part-title-content d) d ri)))))
               null)
         ,@(render-flow* (part-blocks d) d ri #f #f)
         ;; unpack the parts as we go
         ,@(let loop ([pos 1]
                      [secs (part-parts d)])
             (match secs
               ['() '()]
               [(list this-part rest-parts ...)
                (append (render-part this-part ri)
                        (loop (add1 pos) rest-parts))])))))

    (define/override (render-nested-flow t part ri starting-item?)
      (define (super-render)
        (apply append
               (super render-nested-flow t part ri starting-item?)))
      (match t
        [(nested-flow (style "SCentered" _) _)
         (parameterize ([in-centered? #t]
                        [document-main-paragraphs? #f])
           (super-render))]
        [_
         (parameterize ([in-quotation? #t]
                        [document-main-paragraphs? #f])
           (super-render))]))

    (define/private (render-flow* p part ri starting-item? special-last?)
      (let loop ([f p] [starting-item? starting-item?])
        (cond
          [(null? f) null]
          [else (append (render-block (car f) part ri starting-item?)
                        (loop (cdr f) #f))])))

    (define/override (render-flow p part ri starting-item?)
      (render-flow* p part ri starting-item? #t))

    (define/override (render-one p ri fn)
      (parameterize ([tmp-dir
                      (make-temporary-file "scribble-odt~a"
                                           'directory)])
        (define (write-content-xml op)
          (displayln xml-header op)
          (xml:write-xexpr
           `(office:document-content
             ((office:version "1.2")
              ,@xmlns-props)
             (office:scripts)
             ,default-font-decls
             (office:body
              (office:text
               ((text:use-soft-page-breaks "true"))
               ,default-sequence-decls
               ,@(render-part p ri))))
           op))
        (define (build-it)
          (parameterize ([installed-images (mutable-set)])
            ;; write out content.xml
            (call-with-output-file (build-path (tmp-dir) "content.xml")
              write-content-xml)
            ;; write out styles.xml
            (call-with-output-file (build-path (tmp-dir) "styles.xml")
              (lambda (op)
                (displayln xml-header op)
                (xml:write-xexpr default-styles op)))
            ;; write out mimetype
            (call-with-output-file (build-path (tmp-dir) "mimetype")
              (lambda (op) (display odt-mimetype op)))
            ;; Make META-INF directory, write out manifest
            (make-directory (build-path (tmp-dir) "META-INF"))
            (call-with-output-file (build-path (tmp-dir) "META-INF" "manifest.xml")
              (lambda (op)
                (displayln xml-header op)
                (xml:write-xexpr (make-manifest
                                  #:images (sequence->list (installed-images)))
                                 op)))
            (when (file-exists? fn)
              (delete-file fn))
            ;; zip everything up
            (let ([out-filename (path->complete-path fn)])
              (parameterize ([current-directory (tmp-dir)])
                (apply zip out-filename
                       "content.xml"
                       "styles.xml"
                       "mimetype"
                       (build-path "META-INF" "manifest.xml")
                       (map (match-lambda
                              [(vector 'img image-fname mimetype)
                               (string-append "Images/" image-fname)])
                            (sequence->list (installed-images))))))))
        (define (clean-up)
          (delete-directory/files (tmp-dir)))

        (dynamic-wind
          (const #f)
          build-it
          clean-up)
        (void)))

    ;; We're not writing out to a port... we're writing a bunch of
    ;; different things out to zipfiles.
    (define/override (render ds fns ri)
      (for/list ([d ds]
                 [fn fns])
        (when (report-output?) (printf " [Output to ~a]\n" fn))
        (render-one d ri fn)
        ;; apparently we have to return a list of equal length, so...
        #f))

    (define/override (render-other c enclosing-p ri)
      (match c
        ['mdash '("—")]
        ['ndash '("–")]
        ['ldquo '("“")]
        ['rdquo '("”")]
        ['lsquo '("‘")]
        ['rsquo '("’")]
        ['lang ">"]
        ['rang "<"]
        ['larr '("<-")]
        ['rarr '("->")]
        ['prime '("'")]
        ['alpha "\u03B1"]
        ['infin "\u221E"]
        [_ (super render-other c enclosing-p ri)]))

    (define/override (render-table t part ri starting-item?)
      (match t
        ;; Deal with the horrible thing that is scribble's abuse of bibliography
        ;; entries
        [(table (style "AutoBibliography" _) biblio-rows)
         (leave-main-para
          (define pre-paras
            (apply append biblio-rows))
          (apply append
                 (for/list ([pre-para pre-paras])
                   `((text:p ((text:style-name "BiblioEntry"))
                             ,@(super render-paragraph pre-para part ri))))))]
        ;; otherwise yeah
        [_
         (leave-main-para
          (define num-columns 0)
          (define rows
            (for/list ([row (table-blockss t)])
              (define row-len (length row))
              (when (> row-len num-columns)
                (set! num-columns row-len))
              `(table:table-row
                ,@(for/list ([cell row])
                    `(table:table-cell ,@(render-block cell part ri #f))))))

          `((table:table
             ;; declare all the columns this table has
             ,@(for/list ([c num-columns])
                 '(table:table-column))
             ,@rows)))]))

    (super-new)))


(define (make-manifest #:images [images '()])
  `(manifest:manifest
    ((manifest:version "1.2")
     (xmlns:manifest "urn:oasis:names:tc:opendocument:xmlns:manifest:1.0"))
    (manifest:file-entry
     ((manifest:version "1.2")
      (manifest:media-type "application/vnd.oasis.opendocument.text")
      (manifest:full-path "/")))
    (manifest:file-entry
     ((manifest:media-type "text/xml") (manifest:full-path "content.xml")))
    (manifest:file-entry
     ((manifest:media-type "text/xml") (manifest:full-path "styles.xml")))
    (manifest:file-entry
     ((manifest:media-type "text/xml") (manifest:full-path "meta.xml")))
    ,@(for/list ([image-desc images])
        (match image-desc
          [(vector 'img image mimetype)
           `(manifest:file-entry ((manifest:media-type ,mimetype)
                                  (manifest:full-path ,(format "Images/~a" image))))]))))

(define odt-mimetype
  "application/vnd.oasis.opendocument.text")


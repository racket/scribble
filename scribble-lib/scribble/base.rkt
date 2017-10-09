#lang scheme/base

(require "decode.rkt"
         "core.rkt"
         "manual-struct.rkt"
         "decode-struct.rkt"
         "html-properties.rkt"
         "tag.rkt"
         "private/tag.rkt"
         scheme/list
         scheme/class
         racket/contract/base
         racket/contract/combinator
         (for-syntax scheme/base))

(provide (all-from-out "tag.rkt"))

;; ----------------------------------------

(define-syntax-rule (title-like-contract)
  (->* ()
       (#:tag (or/c #f string? (listof string?))
              #:tag-prefix (or/c #f string? module-path?)
              #:style (or/c style? string? symbol? (listof symbol?) #f))
       #:rest (listof pre-content?)
       part-start?))

(provide/contract
 [title (->* ()
             (#:tag (or/c #f string? (listof string?))
                    #:tag-prefix (or/c #f string? module-path?)
                    #:style (or/c style? string? symbol? (listof symbol?) #f)
                    #:version (or/c string? #f)
                    #:date (or/c string? #f))
             #:rest (listof pre-content?)
             title-decl?)]
 [section (title-like-contract)]
 [subsection (title-like-contract)]
 [subsubsection (title-like-contract)]
 [subsubsub*section  (->* ()
                          (#:tag (or/c #f string? (listof string?)))
                          #:rest (listof pre-content?)
                          block?)])
(provide include-section)

(define (title #:tag [tag #f] #:tag-prefix [prefix #f] #:style [style plain]
               #:version [version #f] #:date [date #f]
               . str)
  (let ([content (decode-content str)])
    (make-title-decl (prefix->string prefix)
                     (convert-tag tag content)
                     version
                     (let ([s (convert-part-style 'title style)])
                       (if date
                           (make-style (style-name s)
                                       (cons (make-document-date date)
                                             (style-properties s)))
                           s))
                     content)))

(define (section #:tag [tag #f] #:tag-prefix [prefix #f] #:style [style plain]
                 . str)
  (let ([content (decode-content str)])
    (make-part-start 0 (prefix->string prefix)
                     (convert-tag tag content)
                     (convert-part-style 'section style)
                     content)))

(define (subsection #:tag [tag #f] #:tag-prefix [prefix #f] #:style [style plain]
                    . str)
  (let ([content (decode-content str)])
    (make-part-start 1
                     (prefix->string prefix)
                     (convert-tag tag content)
                     (convert-part-style 'subsection style)
                     content)))

(define (subsubsection #:tag [tag #f] #:tag-prefix [prefix #f]
                       #:style [style plain] . str)
  (let ([content (decode-content str)])
    (make-part-start 2
                     (prefix->string prefix)
                     (convert-tag tag content)
                     (convert-part-style 'subsubsection style)
                     content)))

(define (subsubsub*section #:tag [tag #f] . str)
  (let ([content (decode-content str)])
    (make-paragraph plain 
                    (list
                     (make-element "SSubSubSubSection"
                                   (if tag
                                       (make-target-element #f content `(part ,tag))
                                       content))))))

(define-syntax (include-section stx)
  (syntax-case stx ()
    [(_ mod)
     (with-syntax ([doc-from-mod (datum->syntax #'mod 'doc)])
       (unless (module-path? (syntax->datum #'mod))
         (raise-syntax-error #f
                             "not a module path"
                             stx
                             #'mod))
       #'(begin
           (require (only-in mod [doc-from-mod doc]))
           doc))]))

;; ----------------------------------------

(provide/contract 
 [author (->* (content?) () #:rest (listof content?) block?)]
 [author+email (->* (content? string?) (#:obfuscate? any/c) element?)])

(define (author . auths)
  (make-paragraph 
   (make-style 'author null)
   (let ([nl (make-element 'newline '("\n"))])
     (case (length auths)
       [(1) auths]
       [(2) (list (car auths) nl "and " (cadr auths))]
       [else (let ([r (reverse auths)])
               (append (add-between (reverse (cdr r))
                                    (make-element #f (list "," nl)))
                       (list "," nl "and " (car r))))]))))

(define (author+email name email #:obfuscate? [obfuscate? #f])
  (make-element #f
                (list
                 name
                 " <" 
                 (if obfuscate?
                     (regexp-replace* #rx"[.]"
                                      (regexp-replace* #rx"@" email " at ")
                                      " dot ")
                     (hyperlink (string-append "mailto:" email) email))
                 ">")))

;; ----------------------------------------

(define (item? x) (an-item? x))

(define recur-items/c
  (make-flat-contract 
   #:name 'items/c
   #:first-order (lambda (x)
                   ((flat-contract-predicate items/c) x))))

(define items/c (or/c item?
                      block?
                      (listof recur-items/c)
                      (spliceof recur-items/c)))
                       
(provide items/c)

(provide/contract 
 [itemlist (->* () 
                (#:style (or/c style? string? symbol? #f)) 
                #:rest (listof items/c)
                itemization?)]
 [item (->* () 
            () 
            #:rest (listof pre-flow?)
            item?)])
(provide/contract
 [item? (any/c . -> . boolean?)])

(define (itemlist #:style [style plain] . items)
  (let ([flows (let loop ([items items])
                 (cond
                  [(null? items) null]
                  [(item? (car items)) (cons (an-item-flow (car items))
                                             (loop (cdr items)))]
                  [(block? (car items)) (cons (list (car items))
                                              (loop (cdr items)))]
                  [(splice? (car items))
                   (loop (append (splice-run (car items))
                                 (cdr items)))]
                  [else
                   (loop (append (car items) (cdr items)))]))])
    (make-itemization (convert-block-style style) flows)))

(define-struct an-item (flow))

(define (item . str)
  (make-an-item (decode-flow str)))

;; ----------------------------------------

(provide ._ .__ ~ ?- -~-)

(define ._ (make-element (make-style "Sendabbrev" null) "."))
(define .__ (make-element (make-style "Sendsentence" null) "."))
(define ~ "\uA0")
(define ?- "\uAD")
(define -~- "\u2011")

;; ----------------------------------------

(define elem-like-contract
  (->* () () #:rest (listof pre-content?) element?))

(provide/contract
 [linebreak (-> element?)]
 [nonbreaking elem-like-contract]
 [hspace (-> exact-nonnegative-integer? element?)]
 [elem (->* ()
            (#:style element-style?)
            #:rest (listof pre-content?)
            element?)]
 [italic elem-like-contract]
 [bold elem-like-contract]
 [smaller elem-like-contract]
 [larger elem-like-contract]
 [emph elem-like-contract]
 [tt elem-like-contract]
 [subscript elem-like-contract]
 [superscript elem-like-contract]

 [literal (->* (string?) () #:rest (listof string?) element?)]

 [image (->* ((or/c path-string? (cons/c 'collects (listof bytes?))))
             (#:scale real?
                      #:suffixes (listof (and/c string? #rx"^[.]"))
                      #:style element-style?)
             #:rest (listof content?)
             image-element?)])

(define hspace-cache (make-vector 100 #f))

(define (hspace n)
  (if (n . < . (vector-length hspace-cache))
      (or (vector-ref hspace-cache n)
          (let ([h (make-element 'hspace (list (make-string n #\space)))])
            (vector-set! hspace-cache n h)
            h))
      (make-element 'hspace (list (make-string n #\space)))))

(define (linebreak)
  (make-element 'newline '("\n")))

(define (nonbreaking . str)
  (make-element 'no-break (decode-content str)))

(define (elem #:style [style plain] . str)
  (make-element style (decode-content str)))

(define (italic . str)
  (make-element 'italic (decode-content str)))

(define (bold . str)
  (make-element 'bold (decode-content str)))

(define (smaller . str)
  (make-element 'smaller (decode-content str)))

(define (larger . str)
  (make-element 'larger (decode-content str)))

(define (emph . str)
  (make-element 'italic (decode-content str)))

(define (tt . str)
  (let* ([l (decode-content str)]
         [l (let ([m (and (pair? l)
                          (string? (car l))
                          (regexp-match-positions #rx"^ +" (car l)))])
              (if m
                (list* (hspace (- (cdar m) (caar m)))
                       (substring (car l) (cdar m))
                       (cdr l))
                l))])
    (if (andmap string? l)
      (make-element 'tt l)
      (make-element #f (map (lambda (s)
                              (if (or (string? s) (symbol? s))
                                (make-element 'tt (list s))
                                s))
                            l)))))

(define (span-class classname . str)
  (make-element classname (decode-content str)))

(define (subscript . str)
  (make-element 'subscript (decode-content str)))

(define (superscript . str)
  (make-element 'superscript (decode-content str)))

(define (literal s . strs)
  (let ([s (apply string-append s strs)])
    (make-element #f s)))

(define (image #:scale [scale 1.0] 
               filename-relative-to-source
               #:suffixes [suffixes null]
               #:style [style #f]
               . alt)
  (make-image-element style
                      (decode-content alt)
                      filename-relative-to-source
                      suffixes
                      scale))

;; ----------------------------------------

(define (cell-spec/c c)
  (define rc
    (recursive-contract (or/c c
                              empty
                              (cons/c rc rc))))
  rc)

(provide/contract
 [para (->* ()
            (#:style (or/c style? string? symbol? #f ))
            #:rest (listof pre-content?)
            paragraph?)]
 [nested (->* ()
              (#:style (or/c style? string? symbol? #f ))
              #:rest (listof pre-flow?)
              nested-flow?)]
 [compound (->* ()
                (#:style (or/c style? string? symbol? #f ))
                #:rest (listof pre-flow?)
                compound-paragraph?)]
 [tabular (->* ((listof (listof (or/c 'cont block? content?))))
               (#:style (or/c style? string? symbol? #f)
                #:sep (or/c content? block? #f)
                #:column-properties (listof any/c)
                #:row-properties (listof any/c)
                #:cell-properties (listof (listof any/c)))
               table?)])

(define (convert-block-style style)
  (cond
   [(style? style) style]
   [(or (string? style) (symbol? style)) (make-style style null)]
   [else plain]))

(define (nested #:style [style #f] . c)
  (make-nested-flow (convert-block-style style)
                    (decode-flow c)))

(define (para #:style [style #f] . c)
  (make-paragraph (convert-block-style style)
                  (decode-content c)))

(define (compound #:style [style #f] . c)
  (make-compound-paragraph (convert-block-style style)
                           (decode-flow c)))

(define (tabular #:style [style #f]
                 #:sep [sep #f]
                 #:column-properties [column-properties null]
                 #:row-properties [row-properties null]
                 #:cell-properties [cell-properties null]
                 cells)
  (define (nth-str pos)
    (case (modulo pos 10)
      [(1) "st"]
      [(2) "nd"]
      [(3) "rd"]
      [else "th"]))
  (unless (null? cells)
    (let ([n (length (car cells))])
      (for ([row (in-list (cdr cells))]
            [pos (in-naturals 2)])
        (unless (= n (length row))
          (raise-mismatch-error
           'tabular
           (format "bad length (~a does not match first row's length ~a) for ~a~a row: "
                   (length row)
                   n
                   pos
                   (nth-str pos))
           row)))))
  (for ([row (in-list cells)]
        [pos (in-naturals 1)])
    (when (and (pair? row) (eq? (car row) 'cont))
      (raise-mismatch-error
       'tabular
       (format "~a~a row starts with 'cont: " pos (nth-str pos))
       row)))
  (make-table (let ([s (convert-block-style style)])
                (define n-orig-cols (if (null? cells)
                                        0
                                        (length (car cells))))
                (define n-cols (if sep
                                   (max 0 (sub1 (* n-orig-cols 2)))
                                   n-orig-cols))
                (define n-rows (length cells))
                (unless (null? cells)
                  (when ((length column-properties) . > . n-orig-cols)
                    (raise-mismatch-error
                     'tabular
                     "column properties list is too long: "
                     column-properties)))
                (when ((length row-properties) . > . n-rows)
                  (raise-mismatch-error
                   'tabular
                   "row properties list is too long: "
                   row-properties))
                (when ((length cell-properties) . > . n-rows)
                  (raise-mismatch-error
                   'tabular
                   "cell properties list is too long: "
                   cell-properties))
                (unless (null? cells)
                  (for ([row (in-list cell-properties)])
                    (when ((length row) . > . n-orig-cols)
                      (raise-mismatch-error
                       'tabular
                       "row list within cell properties list is too long: "
                       row))))
                ;; Expand given column and cell properties lists to match
                ;; the dimensions of the given `cells` by duplicating
                ;; the last element of a list as needed (and ignoring
                ;; extra elements):
                (define (make-full-column-properties column-properties)
                  (let loop ([column-properties column-properties]
                             [n 0]
                             [prev null])
                    (cond
                     [(= n n-cols) null]
                     [(null? column-properties)
                      (if (or (zero? n) (not sep))
                          (cons prev (loop null (add1 n) prev))
                          (list* prev prev (loop null (+ n 2) prev)))]
                     [else
                      (define (to-list v) (if (list? v) v (list v)))
                      (define props (to-list (car column-properties)))
                      (define rest (loop (cdr column-properties)
                                         (if (or (zero? n) (not sep))
                                             (add1 n)
                                             (+ n 2))
                                         props))
                      (if (or (zero? n) (not sep))
                          (cons props rest)
                          (list* null props rest))])))
                (define full-column-properties
                  (make-full-column-properties column-properties))
                (define (make-full-cell-properties cell-properties)
                  (let loop ([cell-properties cell-properties]
                             [n 0]
                             [prev (make-list n-cols null)])
                    (cond
                     [(= n n-rows) null]
                     [(null? cell-properties)
                      (cons prev (loop null (add1 n) prev))]
                     [else
                      (define props (make-full-column-properties (car cell-properties)))
                      (cons props
                            (loop (cdr cell-properties)
                                  (add1 n)
                                  props))])))
                (define full-cell-properties
                  (for/list ([c-row (in-list (make-full-cell-properties cell-properties))]
                             [r-row (in-list (make-full-cell-properties (map list row-properties)))])
                    (for/list ([c (in-list c-row)]
                               [r (in-list r-row)])
                      (append c r))))
                (define all-cell-properties
                  (and (or (pair? row-properties)
                           (pair? cell-properties))
                       (if (null? column-properties)
                           full-cell-properties
                           (for/list ([row (in-list full-cell-properties)])
                             (for/list ([cell (in-list row)]
                                        [col (in-list full-column-properties)])
                               (append cell col))))))
                (define all-column-properties
                  (and (pair? column-properties)
                       full-column-properties))
                ;; Will werge `cell-properties` and `column-properties` into
                ;; `s`. Start by finding any existing `table-columns`
                ;; and `table-cells` properties with the right number of
                ;; styles:
                (define props (style-properties s))
                (define tc (and all-column-properties
                                (let ([tc (ormap (lambda (v) (and (table-columns? v) v))
                                                 props)])
                                  (if (and tc
                                           (= (length (table-columns-styles tc))
                                              n-cols))
                                      tc
                                      #f))))
                (define tl (and all-cell-properties
                                (let ([tl (ormap (lambda (v) (and (table-cells? v) v))
                                                 props)])
                                  (if (and tl
                                           (= (length (table-cells-styless tl))
                                              n-rows)
                                           (andmap (lambda (cl)
                                                     (= (length cl) n-cols))
                                                   (table-cells-styless tl)))
                                      tl
                                      #f))))
                ;; Merge:
                (define (cons-maybe v l) (if v (cons v l) l))
                (make-style (style-name s)
                            (cons-maybe
                             (and all-column-properties
                                  (table-columns
                                   (if tc
                                       (for/list ([ps (in-list all-column-properties)]
                                                  [cs (in-list (table-columns-styles tc))])
                                         (make-style (style-name cs)
                                                     (append ps (style-properties cs))))
                                       (for/list ([ps (in-list all-column-properties)])
                                         (make-style #f ps)))))
                             (cons-maybe
                              (and all-cell-properties
                                   (table-cells
                                    (if tl
                                        (for/list ([pss (in-list all-cell-properties)]
                                                   [css (in-list (table-cells-styless tl))])
                                          (for/list ([ps (in-list pss)]
                                                     [cs (in-list css)])
                                            (make-style (style-name cs)
                                                        (append ps (style-properties cs)))))
                                        (for/list ([pss (in-list all-cell-properties)])
                                          (for/list ([ps (in-list pss)])
                                            (make-style #f ps))))))
                              (remq tc (remq tl props))))))
              ;; Process cells:
              (map (lambda (row)
                     (define (cvt cell)
                       (cond
                        [(eq? cell 'cont) cell]
                        [(block? cell) cell]
                        [else (make-paragraph plain cell)]))
                     (define l (map cvt row))
                     (if sep
                         (add-between/cont l (cvt sep))
                         l))
                   cells)))

;; Like `add-between`, but change `sep` to 'cont when
;; adding before a 'cont:
(define (add-between/cont l sep)
  (cond
   [(null? l) null]
   [(null? (cdr l)) l]
   [else
    (list* (car l)
           (if (eq? 'cont (cadr l)) 'cont sep)
           (add-between/cont (cdr l) sep))]))

;; ----------------------------------------

(provide
 (contract-out
  [elemtag (->* ((or/c taglet? generated-tag?))
                ()
                #:rest (listof pre-content?)
                element?)]
  [elemref (->* ((or/c taglet? generated-tag?))
                (#:underline? any/c)
                #:rest (listof pre-content?)
                element?)]
  [secref (->* (string?)
               (#:doc module-path?
                #:tag-prefixes (or/c #f (listof string?))
                #:underline? any/c
                #:link-render-style (or/c #f link-render-style?))
               element?)]
  [Secref (->* (string?)
               (#:doc module-path?
                #:tag-prefixes (or/c #f (listof string?))
                #:underline? any/c
                #:link-render-style (or/c #f link-render-style?))
               element?)]
  [seclink (->* (string?)
                (#:doc module-path?
                 #:tag-prefixes (or/c #f (listof string?))
                 #:underline? any/c
                 #:indirect? any/c)
                #:rest (listof pre-content?)
                element?)]
  [other-doc (->* (module-path?)
                  (#:underline? any/c
                   #:indirect (or/c #f content?))
                  element?)]))

(define (elemtag t . body)
  (make-target-element #f (decode-content body) `(elem ,t)))
(define (elemref #:underline? [u? #t] t . body)
  (make-link-element (if u? #f "plainlink") (decode-content body) `(elem ,t)))

(define (secref s #:underline? [u? #t] #:doc [doc #f] #:tag-prefixes [prefix #f]
                #:link-render-style [link-style #f])
  (make-link-element (let ([name (if u? #f "plainlink")])
                       (if link-style
                           (style name (list link-style))
                           name))
                     null
                     (make-section-tag s #:doc doc #:tag-prefixes prefix)))
(define (Secref s #:underline? [u? #t] #:doc [doc #f] #:tag-prefixes [prefix #f]
                #:link-render-style [link-style #f])
  (let ([le (secref s #:underline? u? #:doc doc #:tag-prefixes prefix)])
    (make-link-element
     (make-style (element-style le) '(uppercase))
     (element-content le)
     (link-element-tag le))))

(define normal-indirect (style #f '(indirect-link)))
(define plain-indirect (style "plainlink" '(indirect-link)))

(define (seclink tag 
                 #:doc [doc #f] 
                 #:underline? [u? #t] 
                 #:tag-prefixes [prefix #f]
                 #:indirect? [indirect? #f]
                 . s)
  (make-link-element (if indirect?
                         (if u?
                             normal-indirect
                             plain-indirect)
                         (if u? 
                             #f 
                             "plainlink"))
                     (decode-content s)
                     `(part ,(doc-prefix doc prefix tag))))

(define (other-doc doc 
                   #:underline? [u? #t]
                   #:indirect [indirect #f])
  (if indirect
      (seclink "top" #:doc doc #:underline? u? #:indirect? #t
               (list "the " indirect " documentation"))
      (secref "top" #:doc doc #:underline? u?)))

;; ----------------------------------------

(provide/contract
 [hyperlink (->* ((or/c string? path?))
                 (#:underline? any/c
                               #:style element-style?)
                 #:rest (listof pre-content?)
                 element?)]
 [url (-> string? element?)]
 [margin-note (->* () (#:left? any/c) #:rest (listof pre-flow?) block?)]
 [margin-note* (->* () (#:left? any/c) #:rest (listof pre-content?) element?)]
 [centered (->* () () #:rest (listof pre-flow?) block?)]
 [verbatim (->* (content?) (#:indent exact-nonnegative-integer?) #:rest (listof content?) block?)])

(define (centered . s)
  (make-nested-flow (make-style "SCentered" null) (decode-flow s)))

(define (hyperlink url
                   #:underline? [underline? #t]
                   #:style [style (if underline? #f "plainlink")]
                   . str)
  (make-element (make-style (if (style? style)
                                (style-name style)
                                style)
                            (cons (make-target-url url)
                                  (if (style? style)
                                      (style-properties style)
                                      null)))
                (decode-content str)))

(define (url str)
  (hyperlink str (make-element 'url str)))

(define (margin-note #:left? [left? #f] . c)
  (make-nested-flow
   (make-style (if left? "refparaleft" "refpara")
               '(command never-indents))
   (list
    (make-nested-flow
     (make-style (if left? "refcolumnleft" "refcolumn")
                 null)
     (list
      (make-nested-flow
       (make-style "refcontent" null)
       (decode-flow c)))))))

(define (margin-note* #:left? [left? #f] . c)
  (make-element
   (make-style (if left? "refelemleft" "refelem") null)
   (make-element
    (make-style (if left? "refcolumnleft" "refcolumn") null)
    (make-element
     (make-style "refcontent" null)
     (decode-content c)))))

(define (verbatim #:indent [i 0] s . more)
  (define lines 
    ;; Break input into a list of lists, where each inner
    ;; list is a single line. Break lines on "\n" in the
    ;; input strings, while non-string content is treated
    ;; as an element within a line.
    (let loop ([l (cons s more)] [strs null])
      (cond
       [(null? l) (if (null? strs)
                      null
                      (map
                       list
                       (regexp-split
                        #rx"\n"
                        (apply string-append (reverse strs)))))]
       [(string? (car l))
        (loop (cdr l) (cons (car l) strs))]
       [else
        (define post-lines (loop (cdr l) null))
        (define pre-lines (loop null strs))
        (define-values (post-line rest-lines)
          (if (null? post-lines)
              (values null null)
              (values (car post-lines) (cdr post-lines))))
        (define-values (first-lines pre-line)
          (if (null? pre-lines)
              (values null null)
              (values (drop-right pre-lines 1)
                      (last pre-lines))))
        (append first-lines
                (list (append pre-line (list (car l)) post-line))
                rest-lines)])))
  (define (str->elts str)
    ;; Convert a single string in a line to typewriter font,
    ;; and also convert multiple adjacent spaces to `hspace` so
    ;; that the space is preserved exactly:
    (let ([spaces (regexp-match-positions #rx"(?:^| ) +" str)])
      (if spaces
        (list* (make-element 'tt (substring str 0 (caar spaces)))
               (hspace (- (cdar spaces) (caar spaces)))
               (str->elts (substring str (cdar spaces))))
        (list (make-element 'tt (list str))))))
  (define (strs->elts line)
    ;; Convert strings in the line:
    (apply append (map (lambda (e) 
                         (if (string? e) 
                             (str->elts e) 
                             (list e)))
                       line)))
  (define indent
    ;; Add indentation to a line:
    (if (zero? i)
      values
      (let ([hs (hspace i)]) (lambda (line) (cons hs line)))))
  (define (make-nonempty l)
    ;; If a line has no content, then add a single space:
    (if (let loop ([l l])
          (cond
           [(null? l) #t]
           [(equal? "" l) #t]
           [(list? l) (andmap loop l)]
           [(element? l) (loop (element-content l))]
           [(multiarg-element? l) (loop (multiarg-element-contents l))]
           [else #f]))
        (list l (hspace 1))
        l))
  (define (make-line line)
    ;; Convert a list of line elements --- a mixture of strings
    ;; and non-strings --- to a paragraph for the line:
    (let* ([line (indent (strs->elts line))])
      (list (make-paragraph omitable-style (make-nonempty line)))))
  (make-table plain (map make-line lines)))

(define omitable-style (make-style 'omitable null))

;; ----------------------------------------

; XXX unknown contract
(provide get-index-entries)
(provide/contract
 [index-block (-> delayed-block?)]
 [index (((or/c string? (listof string?))) ()  #:rest (listof pre-content?) . ->* . index-element?)]
 [index* (((listof string?) (listof any/c)) ()  #:rest (listof pre-content?) . ->* . index-element?)] ; XXX first any/c wrong in docs 
 [as-index (() () #:rest (listof pre-content?) . ->* . index-element?)]
 [section-index (() () #:rest (listof string?) . ->* . part-index-decl?)]
 [index-section (() (#:tag (or/c false/c string?)) . ->* . part?)])

(define (section-index . elems)
  (make-part-index-decl (map content->string elems) elems))

(define (record-index word-seq element-seq tag content)
  (make-index-element #f
                      (list (make-target-element #f content `(idx ,tag)))
                      `(idx ,tag)
                      word-seq
                      element-seq
                      #f))

(define (index* word-seq content-seq . s)
  (let ([key (make-generated-tag)])
    (record-index (map clean-up-index-string word-seq)
                  content-seq key (decode-content s))))

(define (index word-seq . s)
  (let ([word-seq (if (string? word-seq) (list word-seq) word-seq)])
    (apply index* word-seq word-seq s)))

(define (as-index . s)
  (let ([key (make-generated-tag)]
        [content (decode-content s)])
    (record-index
     (list (clean-up-index-string (content->string content)))
     (if (= 1 (length content)) content (list (make-element #f content)))
     key
     content)))

(define (index-section #:title [title "Index"] #:tag [tag #f])
  (make-part #f
             `((part ,(or tag "doc-index")))
             (list title)
             (make-style 'index '(unnumbered))
             null
             (list (index-block))
             null))

;; returns an ordered list of (list tag (text ...) (element ...) index-desc)
(define (get-index-entries sec ri)
  (define (compare-lists xs ys <?)
    (let loop ([xs xs] [ys ys])
      (cond [(and (null? xs) (null? ys)) '=]
            [(null? xs) '<]
            [(null? ys) '>]
            [(<? (car xs) (car ys)) '<]
            [(<? (car ys) (car xs)) '>]
            [else (loop (cdr ys) (cdr xs))])))
  ;; string-ci<? as a major key, and string<? next, so "Foo" precedes "foo"
  ;; (define (string*<? s1 s2)
  ;;   (or (string-ci<? s1 s2)
  ;;       (and (not (string-ci<? s2 s1)) (string<? s1 s2))))
  (define (get-desc entry)
    (let ([desc (cadddr entry)])
      (cond [(exported-index-desc? desc)
             (cons 'libs (map (lambda (l)
                                (format "~s" l))
                              (exported-index-desc-from-libs desc)))]
            [(module-path-index-desc? desc) '(mod)]
            [(part-index-desc? desc) '(part)]
            [(delayed-index-desc? desc) '(delayed)]
            [else '(#f)])))
  ;; parts first, then modules, then bindings, delayed means it's not
  ;; the last round, and #f means no desc
  (define desc-order '(part mod libs delayed #f))
  ;; this defines an imposed ordering for module names
  (define lib-order '(#rx"^racket(?:/|$)" #rx"^r.rs(?:/|$)" #rx"^lang(?:/|$)"))
  (define (lib<? lib1 lib2)
    (define (lib-level lib)
      (let loop ([i 0] [rxs lib-order])
        (if (or (null? rxs) (regexp-match? (car rxs) lib))
          i (loop (add1 i) (cdr rxs)))))
    (let ([l1 (lib-level lib1)] [l2 (lib-level lib2)])
      (if (= l1 l2) (string<? lib1 lib2) (< l1 l2))))
  (define (compare-desc e1 e2)
    (let* ([d1 (get-desc e1)] [d2 (get-desc e2)]
           [t1 (car d1)]      [t2 (car d2)])
      (cond [(memq t2 (cdr (memq t1 desc-order))) '<]
            [(memq t1 (cdr (memq t2 desc-order))) '>]
            [else (case t1 ; equal to t2
                    [(part) '=] ; will just compare tags
                    [(mod)  '=] ; the text fields are the names of the modules
                    [(libs) (compare-lists (cdr d1) (cdr d2) lib<?)]
                    [(delayed) '>] ; dosn't matter, will run again
                    [(#f) '=])])))
  (define (entry<? e1 e2)
    (let ([text1 (cadr e1)] [text2 (cadr e2)])
      (case (compare-lists text1 text2 string-ci<?)
        [(<) #t] [(>) #f]
        [else (case (compare-desc e1 e2)
                [(<) #t] [(>) #f]
                [else (case (compare-lists text1 text2 string<?)
                        [(<) #t] [(>) #f]
                        [else
                         ;; (error 'get-index-entries
                         ;;        ;; when this happens, revise this code so
                         ;;        ;; ordering will always be deterministic
                         ;;        "internal error -- unordered entries: ~e ~e"
                         ;;        e1 e2)
                         ;; Instead, just compare the tags
                         (string<? (format "~a" (car e1))
                                   (format "~a" (car e2)))])])])))
  (define l null)
  (hash-for-each
   (let ([parent (collected-info-parent (part-collected-info sec ri))])
     (if parent
       (collected-info-info (part-collected-info parent ri))
       (let ([ci (resolve-info-ci ri)])
         ;; Force all xref info:
         ((collect-info-ext-demand ci) #f ci)
         (collect-info-ext-ht ci))))
   (lambda (k v)
     (when (and (pair? k) (eq? 'index-entry (car k)))
       (let ([v (if (known-doc? v) (known-doc-v v) v)])
         (set! l (cons (cons (cadr k) v) l))))))
  (sort l entry<?))

(define (index-block)
  (define alpha (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  (define (rows . rows)
    (make-table (make-style 'index null)
                (map (lambda (row)
                       (list (make-paragraph plain row)))
                     rows)))
  (define contents
    (lambda (renderer sec ri)
      (define l (get-index-entries sec ri))
      (define manual-newlines? (send renderer index-manual-newlines?))
      (define alpha-starts (make-hasheq))
      (define alpha-row
        (let loop ([i l] [alpha alpha])
          (define (add-letter let l)
            (list* (make-element "nonavigation" (list (string let))) " " l))
          (cond [(null? alpha) null]
                [(null? i) (add-letter (car alpha) (loop i (cdr alpha)))]
                [else
                 (let* ([strs (cadr (car i))]
                        [letter (if (or (null? strs) (string=? "" (car strs)))
                                  #f
                                  (char-upcase (string-ref (car strs) 0)))])
                   (cond [(not letter) (loop (cdr i) alpha)]
                         [(char-ci>? letter (car alpha))
                          (add-letter (car alpha) (loop i (cdr alpha)))]
                         [(char-ci=? letter (car alpha))
                          (hash-set! alpha-starts (car i) letter)
                          (list* (make-element
                                  (make-style #f (list (make-target-url (format "#alpha:~a" letter))))
                                  (list (string (car alpha))))
                                 " "
                                 (loop (cdr i) (cdr alpha)))]
                         [else (loop (cdr i) alpha)]))])))
      (define body
        (let ([br (if manual-newlines? (make-element 'newline '("\n")) "")])
          (map (lambda (i)
                 (let ([e (make-link-element
                           "indexlink"
                           `(,@(add-between (caddr i) ", ") ,br)
                           (car i))])
                   (cond [(hash-ref alpha-starts i #f)
                          => (lambda (let)
                               (make-element
                                (make-style #f (list
                                                (make-url-anchor
                                                 (format "alpha:~a" (char-upcase let)))))
                                (list e)))]
                         [else e])))
               l)))
      (if manual-newlines?
        (rows alpha-row '(nbsp) body)
        (apply rows alpha-row '(nbsp) (map list body)))))
  (make-delayed-block contents))

;; ----------------------------------------

(provide/contract
 [table-of-contents (-> delayed-block?)]
 [local-table-of-contents (() 
                           (#:style (or/c style? string? symbol? (listof symbol?) #f))
                           . ->* . delayed-block?)])

(define (table-of-contents)
  (make-delayed-block
   (lambda (renderer part ri)
     (send renderer table-of-contents part ri))))

(define (local-table-of-contents #:style [style plain])
  (make-delayed-block
   (lambda (renderer part ri)
     (send renderer local-table-of-contents part ri style))))

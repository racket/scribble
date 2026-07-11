#lang racket/base
(require "core.rkt"
         "base-render.rkt"
         "private/render-utils.rkt"
         "typst-properties.rkt"
         (only-in "latex-properties.rkt"
                  command-extras?
                  command-extras-arguments)
         racket/class
         racket/port
         racket/list
         racket/string
         racket/runtime-path
         setup/collects
         file/convertible)

(provide render-mixin
         make-render-part-mixin)

(define current-indent (make-parameter 0))
(define (make-indent amt)
  (+ amt (current-indent)))
(define (indent)
  (define i (current-indent))
  (unless (zero? i) (display (make-string i #\space))))
(define (indented-newline)
  (newline)
  (indent))

(define in-link? (make-parameter #f))
;; True while rendering content within a table cell, so that a nested
;; table can adjust its alignment:
(define in-table-cell? (make-parameter #f))
;; True while rendering content whose spacing and characters should
;; be kept verbatim, such as code: spaces are converted to
;; non-breaking spaces, and ligature-like substitutions (quotes and
;; dashes) are disabled:
(define preserving-spaces? (make-parameter #f))

(define typst-part-tag 'typst-section)

(define-runtime-path scribble-prefix-typ "scribble-prefix.typ")
(define-runtime-path scribble-typ "scribble.typ")
(define-runtime-path scribble-style-typ "scribble-style.typ")

;; Code-token styles (from "racket.rkt") that have a corresponding
;; function definition in "racket.typ" and whose content should be
;; rendered with verbatim spacing; other string style names are
;; also rendered as function calls, but without verbatim spacing:
(define rkt-style-names
  '("RktPlain" "RktBlk" "RktIn" "RktInBG" "RktRdr" "RktPn" "RktMeta"
    "RktMod" "RktKw" "RktOpt" "RktErr" "RktVar" "RktSym" "RktSymDef"
    "RktValLink" "RktValDef" "RktModLink" "RktStxLink" "RktStxDef"
    "RktRes" "RktOut" "RktCmt" "RktVal" "highlighted"))

;; Escape a string for use in Typst markup mode. The escaped set
;; covers all characters that are (or can be) markup-significant;
;; parentheses and `.` are included because, immediately after an
;; embedded code expression, an open parenthesis would be parsed as
;; a function call and a `.` as a field access.
(define (typst-escape s)
  (regexp-replace* #rx"[][()\\{}#$*`@<>/~'\".=+_-]" s "\\\\&"))

;; Escape a string for use inside a Typst string literal:
(define (typst-string-escape s)
  (let* ([s (regexp-replace* #rx"\\\\" s "\\\\\\\\")]
         [s (regexp-replace* #rx"\"" s "\\\\\"")]
         [s (regexp-replace* #rx"\n" s "\\\\n")]
         [s (regexp-replace* #rx"\t" s "\\\\t")]
         [s (regexp-replace* #rx"\r" s "")])
    s))

;; Encode a tag as a Typst label name; produces only alphanumeric
;; ASCII characters plus `_`, which are all allowed in label names:
(define (t-encode s)
  (string-append*
   (map (lambda (c)
          (cond
            [(and (or (char-alphabetic? c) (char-numeric? c))
                  ((char->integer c) . < . 128))
             (string c)]
            [(char=? c #\space) "_"]
            [else (format "x~x" (char->integer c))]))
        (string->list (format "~s" s)))))

;; Color names that Typst predefines, for `color-property` values
;; that arrive as strings:
(define typst-color-names
  '("black" "gray" "silver" "white" "navy" "blue" "aqua" "teal" "eastern"
    "purple" "fuchsia" "maroon" "red" "orange" "yellow" "olive" "green" "lime"))

(define (typst-color c)
  (cond
    [(string? c) (if (member c typst-color-names) c "black")]
    [else (format "rgb(~a, ~a, ~a)" (car c) (cadr c) (caddr c))]))

;; Typst rejects an SVG image that declares a zero width or height,
;; so rewrite each zero dimension --- in the root tag's attributes
;; and its "viewBox" --- to 1, and report the growth in points so
;; that the image's layout size can be adjusted to compensate;
;; returns the (possibly updated) SVG bytes and either #f or an
;; inset-adjustment string like "bottom: -0.75pt":
(define (patch-zero-size-svg bs)
  (define (svg-unit->pt u)
    (cond
      [(or (equal? u #"") (equal? u #"px")) 0.75]
      [(equal? u #"pt") 1]
      [(equal? u #"pc") 12]
      [(equal? u #"mm") 2.834646]
      [(equal? u #"cm") 28.34646]
      [(equal? u #"in") 72]
      [else 0.75]))
  (define m (regexp-match-positions #rx#"<svg[^>]*>" bs))
  (cond
    [(not m) (values bs #f)]
    [else
     (define tag (subbytes bs (caar m) (cdar m)))
     ;; Replace a zero `width' or `height' attribute with 1 (in the
     ;; same unit), and return the growth in points:
     (define (patch-dim tag which)
       (define rx (byte-pregexp (bytes-append which #"=\"([0-9.eE+-]+)([a-z%]*)\"")))
       (define dim (regexp-match rx tag))
       (cond
         [(and dim (zero? (string->number (bytes->string/utf-8 (cadr dim)))))
          (values (regexp-replace rx tag (bytes-append which #"=\"1" (caddr dim) #"\""))
                  (svg-unit->pt (caddr dim)))]
         [else (values tag #f)]))
     ;; Replace a zero width or height in a `viewBox' attribute with
     ;; 1, so that it stays consistent with the patched dimensions:
     (define (patch-viewbox tag)
       (define rx #px#"(viewBox=\"[0-9.eE+-]+[ ,]+[0-9.eE+-]+[ ,]+)([0-9.eE+-]+)([ ,]+)([0-9.eE+-]+)(\")")
       (define vb (regexp-match rx tag))
       (define (nonzero n) (if (zero? (string->number (bytes->string/utf-8 n))) #"1" n))
       (cond
         [vb (regexp-replace rx tag (bytes-append (cadr vb)
                                                  (nonzero (caddr vb))
                                                  (cadddr vb)
                                                  (nonzero (list-ref vb 4))
                                                  (list-ref vb 5)))]
         [else tag]))
     (define-values (w-tag trim-w) (patch-dim tag #"width"))
     (define-values (h-tag trim-h) (patch-dim w-tag #"height"))
     (cond
       [(or trim-w trim-h)
        (values (bytes-append (subbytes bs 0 (caar m))
                              (patch-viewbox h-tag)
                              (subbytes bs (cdar m)))
                (string-append*
                 (append (if trim-w (list (format "right: -~apt" trim-w)) null)
                         (if (and trim-w trim-h) '(", ") null)
                         (if trim-h (list (format "bottom: -~apt" trim-h)) null))))]
       [else (values bs #f)])]))

(define-struct (toc-paragraph paragraph) ())

(define (render-mixin %)
  (class %
    (super-new)

    (inherit-field prefix-file style-file style-extra-files image-preferences)

    (define/override (current-render-mode)
      '(typst))

    (define/override (get-suffix) #".typ")

    (define/override (get-substitutions)
      '((#rx"---" "\U2014")
        (#rx"--" "\U2013")
        (#rx"``" "\U201C")
        (#rx"''" "\U201D")
        (#rx"'" "\U2019")))

    (inherit render-block
             render-part
             install-file
             format-number
             number-depth
             sort-image-requests
             extract-version
             extract-date
             extract-authors
             extract-pretitle-content
             extract-part-style-files)

    (define image-reqs
      (sort-image-requests '(svg-bytes png@2x-bytes png-bytes pdf-bytes)
                           image-preferences))

    ;; Typst reports an error for a reference to a label that is not
    ;; defined or that is defined multiple times, so keep track of
    ;; emitted and referenced labels; extra anchors are added at the
    ;; end of the document for referenced labels that were never
    ;; emitted (e.g., a target that appears only inside a code block)
    (define emitted-labels (make-hash))
    (define linked-labels (make-hash))

    ;; Emits a `#metadata' anchor for `lbl' (unless one was already
    ;; emitted for it), which is invisible but linkable:
    (define/private (emit-anchor lbl)
      (unless (hash-ref emitted-labels lbl #f)
        (hash-set! emitted-labels lbl #t)
        (printf "#metadata(none)<~a>" lbl)))

    (define/public (render-part-depth) #f)

    ;; ----------------------------------------
    ;; collect

    (define/override (collect-part-tags d ci number)
      (for ([t (part-tags d)])
        (let ([t (generate-tag t ci)])
          (collect-put! ci
                        t
                        (vector (or (part-title-content d) '("???"))
                                (add-current-tag-prefix t)
                                number
                                typst-part-tag)))))

    ;; ----------------------------------------
    ;; render

    (define/override (render-one d ri fn)
      (define defaults (ormap (lambda (v) (and (typst-defaults? v) v))
                              (style-properties (part-style d))))
      (let* ([prefix-file (or prefix-file
                              (and defaults
                                   (let ([v (typst-defaults-prefix defaults)])
                                     (cond
                                       [(bytes? v) v]
                                       [else (collects-relative->path v)])))
                              scribble-prefix-typ)]
             [style-file (or style-file
                             (and defaults
                                  (let ([v (typst-defaults-style defaults)])
                                    (cond
                                      [(bytes? v) v]
                                      [else (collects-relative->path v)])))
                             scribble-style-typ)]
             [all-style-files (list* prefix-file
                                     scribble-typ
                                     (append (extract-part-style-files
                                              d
                                              ri
                                              (lambda (p) #f)
                                              typ-addition?
                                              typ-addition-path)
                                             (list style-file)
                                             style-extra-files))])
        (unless (render-part-depth)
          (define (copy-file-to-output file)
            (if (bytes? file)
                (display file)
                (with-input-from-file file
                  (lambda ()
                    (copy-port (current-input-port) (current-output-port))))))
          (for ([style-file (in-list all-style-files)])
            (if (bytes? style-file)
                (display style-file)
                (with-input-from-file style-file
                  (lambda ()
                    (copy-port (current-input-port) (current-output-port))))))
          (define title-content (part-title-content d))
          (define title-str (and title-content (content->string title-content this d ri)))
          (when (and title-str (not (equal? title-str "")))
            (printf "#set document(title: \"~a\")\n" (typst-string-escape title-str)))
          (newline)
          (when (and title-content
                     (not (and (part-style? d 'hidden)
                               (equal? "" title-str))))
            (let ([vers (extract-version d)]
                  [date (extract-date d)]
                  [auths (extract-authors d)]
                  [pres (extract-pretitle-content d)])
              (for ([pre (in-list pres)])
                (newline)
                (cond
                  [(paragraph? pre) (do-render-paragraph pre d ri #t)]
                  [(nested-flow? pre) (do-render-nested-flow pre d ri #f #t)]))
              (printf "#Stitle(title: [")
              (render-content title-content d ri)
              (printf "]")
              (unless (equal? vers "")
                (printf ",\n version: SVersion[~a]" (typst-escape vers)))
              (printf ",\n authors: (")
              (for ([auth (in-list auths)])
                (printf "[")
                (do-render-paragraph auth d ri #t)
                (printf "],"))
              (printf ")")
              (when date
                (printf ",\n date: [~a]" (typst-escape date)))
              (printf ")")
              (newline))))
        (render-part d ri)
        (unless (render-part-depth)
          ;; Add anchors for any referenced-but-never-emitted labels, so
          ;; that no `#link' in the document can fail to resolve:
          (let ([missing (sort (for/list ([lbl (in-hash-keys linked-labels)]
                                          #:unless (hash-ref emitted-labels lbl #f))
                                 lbl)
                               string<?)])
            (unless (null? missing)
              (newline)
              (for ([lbl (in-list missing)])
                (printf "#metadata(none) <~a>\n" lbl)))))))

    (define/override (render-part-content d ri)
      (define number (collected-info-number (part-collected-info d ri)))
      (define depth (+ (number-depth number) (or (render-part-depth) 0)))
      (define hidden? (or (and (part-style? d 'hidden)
                               (equal? (content->string (part-title-content d)) ""))
                          (zero? depth)))
      (cond
        [hidden?
         ;; No heading, but emit anchors so that links to this part work:
         (for ([t (part-tags d)])
           (emit-anchor (t-encode (add-current-tag-prefix (tag-key t ri))))
           (newline))]
        [else
         (printf "#Sheading(depth: ~a~a~a)["
                 depth
                 (if (part-style? d 'toc-hidden)
                     ", outlined: false"
                     "")
                 (if (part-style? d 'hidden)
                     ", hidden: true"
                     ""))
         (let ([s (format-number number '() #t)])
           (unless (null? s)
             (printf "~a~a" (typst-escape (car s)) (if (part-title-content d) " " ""))))
         (when (part-title-content d)
           (render-content (part-title-content d) d ri))
         (printf "]\n")
         ;; Only one label can attach to the heading; emit any others
         ;; as invisible anchors:
         (for ([t (part-tags d)]
               [i (in-naturals)])
           (let ([lbl (t-encode (add-current-tag-prefix (tag-key t ri)))])
             (cond
               [(and (zero? i) (not (hash-ref emitted-labels lbl #f)))
                (hash-set! emitted-labels lbl #t)
                (printf " <~a>" lbl)]
               [else
                (newline)
                (emit-anchor lbl)])))
         (newline)
         (newline)])
      (render-flow (part-blocks d) d ri #f)
      (for ([sec (part-parts d)])
        (newline)
        (render-part sec ri))
      null)

    (define/override (render-flow f part ri starting-item?)
      (if (null? f)
          null
          (append*
           (render-block (car f) part ri starting-item?)
           (for/list ([p (in-list (cdr f))])
             (indented-newline)
             (render-block p part ri #f)))))

    (define/override (render-intrapara-block p part ri first? last? starting-item?)
      (unless first? (indented-newline))
      (super render-intrapara-block p part ri first? last? starting-item?))

    ;; ----------------------------------------
    ;; paragraphs

    (define/override (render-paragraph p part ri)
      (do-render-paragraph p part ri #f))

    (define/private (do-render-paragraph p part ri show-pre?)
      (define sn (style-name (paragraph-style p)))
      (cond
        [(and (not show-pre?)
              (or (eq? sn 'author) (eq? sn 'pretitle)))
         null]
        [(toc-paragraph? p)
         (printf "#outline()")
         (newline)
         null]
        [else
         (when (string? sn) (printf "#~a[" sn))
         (super render-paragraph p part ri)
         (when (string? sn) (printf "]"))
         (unless show-pre? (newline))
         null]))

    (define/override (table-of-contents part ri)
      (make-toc-paragraph plain null))

    (define/override (local-table-of-contents part ri style)
      (make-paragraph plain null))

    ;; ----------------------------------------
    ;; content

    (define/private (content-style e)
      (cond
        [(element? e) (element-style e)]
        [(multiarg-element? e) (multiarg-element-style e)]
        [else #f]))

    (define/private (content-style-name e)
      (define s (content-style e))
      (if (style? s) (style-name s) s))

    ;; For a style with a function definition in "racket.typ", the
    ;; matching function name; 'tt and other code-like styles map
    ;; to the plain monospace function:
    (define (code-style-function i)
      (define sn (content-style-name i))
      (cond
        [(and (string? sn) (member sn rkt-style-names)) sn]
        [(or (eq? sn 'tt)
             (eq? sn 'url))
         "Stt"]
        [else #f]))

    (define (preserve-spaces? i)
      (eq? 'hspace (content-style-name i)))

    (define (find-target-url i)
      (define s (content-style i))
      (and (style? s)
           (for/or ([v (in-list (style-properties s))])
             (and (target-url? v) v))))

    (define/override (render-content e part ri)
      (when (target-element? e)
        (emit-anchor (t-encode (add-current-tag-prefix
                                (tag-key (target-element-tag e) ri)))))
      (define link-target
        (and (not (in-link?))
             (or (let ([u (find-target-url e)])
                   (and u (target-url-addr u)))
                 (and (link-element? e)
                      (let-values ([(dest ext?)
                                    (resolve-get/ext? part ri (link-element-tag e))])
                        (and dest
                             (not ext?)
                             (let ([lbl (t-encode (vector-ref dest 1))])
                               (hash-set! linked-labels lbl #t)
                               (list 'label lbl))))))))
      (cond
        [link-target
         (if (pair? link-target)
             (printf "#link(label(\"~a\"))[" (cadr link-target))
             (printf "#link(\"~a\")[" (typst-string-escape
                                       (let ([p link-target])
                                         (if (path? p) (path->string p) p)))))
         (begin0
           (parameterize ([in-link? #t])
             (render-styled e part ri))
           (display "]"))]
        [else (render-styled e part ri)]))

    (define/private (render-styled e part ri)
      (define es (content-style e))
      (define props (if (style? es) (style-properties es) null))
      (define color (findf color-property? props))
      (define bg (findf background-color-property? props))
      (when color
        (printf "#text(fill: ~a)[" (typst-color (color-property-color color))))
      (when bg
        (printf "#highlight(fill: ~a)[" (typst-color (background-color-property-color bg))))
      (begin0
        (render-plain-styled e part ri)
        (when bg (display "]"))
        (when color (display "]"))))

    (define/private (render-plain-styled e part ri)
      (define sn (content-style-name e))
      (define (wrap pre post)
        (display pre)
        (begin0
          (super render-content e part ri)
          (display post)))
      (cond
        [(and (image-element? e))
         (let ([fn (install-file
                    (select-suffix
                     (collects-relative->path (image-element-path e))
                     (image-element-suffixes e)
                     '(".svg" ".png" ".pdf")))])
           (render-image fn (image-element-scale e) #f)
           null)]
        [(and (convertible? e)
              (render-convertible e))
         null]
        [(code-style-function e)
         => (lambda (fn)
              (printf "#~a[" fn)
              (begin0
                (parameterize ([preserving-spaces? #t])
                  (super render-content e part ri))
                (display "]")))]
        [(and (preserve-spaces? e) (not (preserving-spaces?)))
         ;; The monospace wrapper makes the width of the preserved
         ;; spaces match surrounding code (as in "scribble.css"):
         (display "#Stt[")
         (begin0
           (parameterize ([preserving-spaces? #t])
             (render-content e part ri))
           (display "]"))]
        [(eq? sn 'bold) (wrap "#strong[" "]")]
        [(or (eq? sn 'italic) (eq? sn 'emph)) (wrap "#emph[" "]")]
        [(eq? sn 'subscript) (wrap "#sub[" "]")]
        [(eq? sn 'superscript) (wrap "#super[" "]")]
        [(eq? sn 'smaller) (wrap "#text(size: 0.83em)[" "]")]
        [(eq? sn 'larger) (wrap "#text(size: 1.2em)[" "]")]
        [(eq? sn 'no-break) (wrap "#box[" "]")]
        [(eq? sn 'newline)
         (display "#linebreak()")
         null]
        [(string? sn)
         (cond
           [(multiarg-element? e)
            (printf "#~a" sn)
            (for ([l (in-list (multiarg-element-contents e))])
              (printf "[")
              (render-content l part ri)
              (printf "]"))
            null]
           [else
            (begin0
              (wrap (string-append "#" sn "[") "]")
              (cond
                [(findf command-extras? (let ([s (content-style e)])
                                          (if (style? s)
                                              (style-properties s)
                                              '())))
                 => (lambda (ce)
                      (for ([l (in-list (command-extras-arguments ce))])
                        (printf "[~a]" l)))]))])]
        [else (super render-content e part ri)]))

    ;; ----------------------------------------
    ;; images

    ;; Wrapping `image' in `box' makes it suitable for inline
    ;; contexts, and it stays a single unit in block contexts; a
    ;; `#:trim' inset adjustment compensates for a dimension that
    ;; was added by `patch-zero-size-svg':
    (define/private (render-image fn scale width #:trim [trim #f])
      (define img
        (format "image(\"~a\"~a)"
                (typst-string-escape (if (path? fn) (path->string fn) fn))
                (if width (format ", width: ~apt" width) "")))
      (printf "#box(~a~a)"
              (if trim (format "inset: (~a), " trim) "")
              (if (and scale (not (= scale 1)))
                  (format "scale(x: ~a%, y: ~a%, reflow: true, ~a)"
                          (exact->inexact (* 100 scale))
                          (exact->inexact (* 100 scale))
                          img)
                  img)))

    (define/private (render-convertible e)
      (for/or ([req (in-list image-reqs)])
        (case req
          [(svg-bytes)
           (let ([v (convert e 'svg-bytes)])
             (and v
                  (let-values ([(v trim) (patch-zero-size-svg v)])
                    (render-image (install-file "pict.svg" v) #f #f #:trim trim)
                    #t)))]
          [(png@2x-bytes)
           (let ([v (convert e 'png@2x-bytes+bounds8)])
             (and v
                  (begin
                    (render-image (install-file "pict.png" (car v)) #f (cadr v))
                    #t)))]
          [(png-bytes)
           (let ([v (convert e 'png-bytes+bounds8)]
                 [plain-v (lambda () (convert e 'png-bytes))])
             (cond
               [v (render-image (install-file "pict.png" (car v)) #f (cadr v)) #t]
               [(plain-v)
                => (lambda (v)
                     (render-image (install-file "pict.png" v) #f #f)
                     #t)]
               [else #f]))]
          [(pdf-bytes)
           (let ([v (convert e 'pdf-bytes)])
             (and v
                  (begin
                    (render-image (install-file "pict.pdf" v) #f #f)
                    #t)))]
          [else #f])))

    ;; ----------------------------------------
    ;; itemizations

    (define/override (render-itemization i part ri)
      (define flows (itemization-blockss i))
      (define enum? (eq? 'ordered (style-name (itemization-style i))))
      (unless (null? flows)
        (displayln (string-append "#"
                                  (if enum? "enum" "list")
                                  "("))
        (for/list ([d (in-list flows)]
                   [i (in-naturals)])
          (unless (zero? i) (displayln ","))
          (display "[")
          (render-flow d part ri #t)
          (display "]"))
        (display ")"))
      null)

    ;; ----------------------------------------
    ;; nested flows

    (define/override (render-nested-flow i part ri starting-item?)
      (do-render-nested-flow i part ri starting-item? #f))

    (define/private (do-render-nested-flow i part ri starting-item? show-pre?)
      (define s (nested-flow-style i))
      (define props (style-properties s))
      (cond
        [(and (not show-pre?) (memq 'pretitle props))
         null]
        [else
         (define sn (style-name s))
         (define block-name
           (cond
             [(eq? sn 'inset) "SInset"]
             [(eq? sn 'code-inset) "SCodeInset"]
             [(eq? sn 'vertical-inset) "SVerticalInset"]
             [(string? sn) sn]
             [else #f]))
         (cond
           [block-name
            (cond
              [(memq 'multicommand props)
               (printf "#~a" block-name)
               (for/list ([b (in-list (nested-flow-blocks i))]
                          [pos (in-naturals)])
                 (printf "[")
                 (render-block b part ri starting-item?)
                 (printf "]"))]
              [else
               (printf "#~a[" block-name)
               (begin0
                 (super render-nested-flow i part ri starting-item?)
                 (printf "]")
                 (newline))])]
           [else
            (super render-nested-flow i part ri starting-item?)])]))

    ;; ----------------------------------------
    ;; tables

    (define/override (render-table i part ri starting-item?)
      (define flowss (table-blockss i))
      (cond
        [(or (null? flowss) (null? (car flowss))) null]
        [else
         ;; Backgrounds as in "racket.css":
         (define table-function
           (let ([s-name (style-name (table-style i))])
             (cond
               [(eq? s-name 'boxed) "Sboxed"]
               [(string? s-name) s-name]
               [else "Stable"])))
         (define cell-styless (extract-table-cell-styles i))
         ;; For a nested table in a `top'-aligned cell, remove the
         ;; top inset of the table's first row, so that the top
         ;; edge of the row's text (instead of the top edge of the
         ;; table) aligns with the text of sibling cells --- which
         ;; approximates the way that a `tabular[t]' environment in
         ;; Latex output aligns on the first row's baseline:
         (define nested? (in-table-cell?))
         (define nonbreakable? (memq 'block (style-properties (table-style i))))
         (when nonbreakable? (printf "#block(breakable: false)["))
         (printf "#~a(\n" table-function)
         (indent)
         (printf "  columns: ~a,\n" (length (car flowss)))
         (for ([row (in-list flowss)]
               [styles (in-list cell-styless)]
               [row-i (in-naturals)])
           (indent)
           (printf "  ")
           (let loop ([row row] [styles styles])
             (unless (null? row)
               (define d (car row))
               (cond
                 [(eq? d 'cont)
                  ;; A 'cont in the first column; render as an empty cell
                  (printf "[], ")
                  (loop (cdr row) (cdr styles))]
                 [else
                  (define cnt (let inner ([row (cdr row)] [n 1])
                                (cond
                                  [(null? row) n]
                                  [(eq? (car row) 'cont) (inner (cdr row) (add1 n))]
                                  [else n])))
                  (define props (style-properties (car styles)))
                  (define opts
                    (append
                     (if (and nested? (zero? row-i)) (list "inset: (top: 0pt)") null)
                     (if (cnt . > . 1) (list (format "colspan: ~a" cnt)) null)
                     (let ([horiz (cond
                                    [(memq 'right props) "right"]
                                    [(memq 'center props) "center"]
                                    [else #f])]
                           [vert (cond
                                   [(memq 'top props) "top"]
                                   [(memq 'bottom props) "bottom"]
                                   [else #f])])
                       (if (or horiz vert)
                           (list (format "align: ~a"
                                         (string-join (filter values (list horiz vert)) " + ")))
                           null))
                     (cond
                       [(memq 'border props) (list "stroke: 0.6pt")]
                       [else
                        (define sides
                          (filter values
                                  (list (and (memq 'left-border props) "left: 0.6pt")
                                        (and (memq 'right-border props) "right: 0.6pt")
                                        (and (memq 'top-border props) "top: 0.6pt")
                                        (and (memq 'bottom-border props) "bottom: 0.6pt"))))
                        (if (null? sides)
                            null
                            (list (format "stroke: (~a)" (string-join sides ", "))))])))
                  (define sn (and (string? (style-name (car styles)))
                                  (style-name (car styles))))
                  (display "table.cell")
                  (if (null? opts)
                      (printf "[")
                      (printf "(~a)[" (string-join opts ", ")))
                  (when sn
                    (printf "#~a[" sn))
                  (define o (open-output-string))
                  (parameterize ([current-indent 0]
                                 [current-output-port o]
                                 [in-table-cell? #t])
                    (render-block d part ri #f))
                  (let ([s (regexp-replace #rx"\n+$" (get-output-string o) "")])
                    ;; A non-breaking space keeps an all-blank row
                    ;; (such as a blank line in a code block) from
                    ;; collapsing:
                    (display (if (regexp-match? #px"^\\s*$" s) "\uA0" s)))
                  (when sn
                    (printf "]"))
                  (printf "], ")
                  (loop (list-tail row cnt) (list-tail styles cnt))])))
           (newline))
         (indent)
         (printf ")")
         (when nonbreakable? (printf "]"))
         (newline)
         null]))

    ;; ----------------------------------------
    ;; strings and other atoms

    (define/override (render-other i part ri)
      (cond
        [(symbol? i)
         (display (case i
                    [(mdash) "—"]
                    [(ndash) "–"]
                    [(ldquo) "“"]
                    [(rdquo) "”"]
                    [(lsquo) "‘"]
                    [(rsquo) "’"]
                    [(prime) "′"]
                    [(rarr) "→"]
                    [(larr) "←"]
                    [(alpha) "α"]
                    [(infin) "∞"]
                    [(lang) "⟨"]
                    [(rang) "⟩"]
                    [(nbsp) "\uA0"]
                    [else (error 'typst-render "unknown element symbol: ~e" i)]))]
        [(string? i)
         (let* ([s i]
                [s (typst-escape s)]
                [s (if (preserving-spaces?)
                       (regexp-replace* #rx" " s "\uA0")
                       s)])
           (display s))]
        [else (render-other (format "~s" i) part ri)])
      null)))

(define (make-render-part-mixin n)
  (lambda (%)
    (class (render-mixin %)
      (define/override (render-part-depth) n)
      (super-new))))

(define (regexp-replace** str ptns&reps)
  (for/fold ([str str])
            ([ptn (in-list (map car ptns&reps))]
             [rep (in-list (map cdr ptns&reps))])
    (regexp-replace* ptn str rep)))

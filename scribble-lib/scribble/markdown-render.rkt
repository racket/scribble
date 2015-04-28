#lang racket/base
(require "core.rkt"
         "base-render.rkt"
         "private/render-utils.rkt"
         racket/class racket/port racket/list racket/string racket/match
         scribble/text/wrap)
(provide render-mixin)

(define current-indent (make-parameter 0))
(define (make-indent amt)
  (+ amt (current-indent)))
(define (indent)
  (define i (current-indent))
  (unless (zero? i) (display (make-string i #\space))))
(define (indented-newline)
  (newline)
  (indent))

(define note-depth (make-parameter 0))

(define (render-mixin %)
  (class %
    
    (define/override (current-render-mode)
      '(markdown))

    (define/override (get-suffix) #".md")

    (define/override (get-substitutions)
      '((#rx"---" "\U2014")
        (#rx"--" "\U2013")
        (#rx"``" "\U201C")
        (#rx"''" "\U201D")
        (#rx"'" "\U2019")))

    (inherit render-block
             format-number
             number-depth)

    (define/override (render-part d ht)
      (let ([number (collected-info-number (part-collected-info d ht))])
        (unless (part-style? d 'hidden)
          (printf (string-append (make-string (add1 (number-depth number)) #\#) " "))
          (let ([s (format-number number '())])
            (unless (null? s)
              (printf "~a.~a" 
                      (car s)
                      (if (part-title-content d)
                          " "
                          "")))
            (when (part-title-content d)
              (render-content (part-title-content d) d ht))
            (when (or (pair? number) (part-title-content d))
              (newline)
              (newline))))
        (render-flow (part-blocks d) d ht #f)
        (let loop ([pos 1]
                   [secs (part-parts d)]
                   [need-newline? (pair? (part-blocks d))])
          (unless (null? secs)
            (when need-newline? (newline))
            (render-part (car secs) ht)
            (loop (add1 pos) (cdr secs) #t)))))

    (define/override (render-flow f part ht starting-item?)
      (if (null? f)
          null
          (append*
           (render-block (car f) part ht starting-item?)
           (for/list ([p (in-list (cdr f))])
             (indented-newline)
             (render-block p part ht #f)))))

    (define/override (render-intrapara-block p part ri first? last? starting-item?)
      (unless first? (indented-newline))
      (super render-intrapara-block p part ri first? last? starting-item?))

    (define/override (render-table i part ht inline?)
      (define flowss (table-blockss i))

      (define tick? (member (style-name (table-style i))
                            (list 'boxed "defmodule" "RktBlk")))

      (cond
        [(null? flowss) null]

        [(and tick? (not (in-code?)))
          (displayln "```racket")
          (parameterize ([in-code? #t])
            (render-table i part ht inline?))
          (displayln "```")]

        [else
          (define strs (map (lambda (flows)
                              (map (lambda (d)
                                     (if (eq? d 'cont)
                                         d
                                         (let ([o (open-output-string)])
                                           (parameterize ([current-indent 0]
                                                          [current-output-port o])
                                             (render-block d part ht #f))
                                           (regexp-split
                                            #rx"\n"
                                            (regexp-replace #rx"\n$"
                                                            (get-output-string o)
                                                            "")))))
                                   flows))
                            flowss))
          (define widths (map (lambda (col)
                                (for/fold ([d 0]) ([i (in-list col)])
                                  (if (eq? i 'cont)
                                      0
                                      (apply max d (map string-length i)))))
                              (apply map list strs)))
          (define x-length (lambda (col) (if (eq? col 'cont) 0 (length col))))
          (for/fold ([indent? #f]) ([row (in-list strs)])
            (let ([h (apply max 0 (map x-length row))])
              (let ([row* (for/list ([i (in-range h)])
                            (for/list ([col (in-list row)])
                              (if (i . < . (x-length col))
                                  (list-ref col i)
                                  "")))])
                (for/fold ([indent? indent?]) ([sub-row (in-list row*)])
                  (when indent? (indent))
                  (for/fold ([space? #f])
                      ([col (in-list sub-row)]
                       [w (in-list widths)])
                    (let ([col (if (eq? col 'cont) "" col)])
                      (display (regexp-replace* #rx"\uA0" col " "))
                      (display (make-string (max 0 (- w (string-length col))) #\space)))
                    #t)
                  (newline)
                  #t)))
            #t)])
      null)

    (define/override (render-itemization i part ht)
      (let ([flows (itemization-blockss i)])
        (if (null? flows)
            null
            (append*
             (begin (printf "* ")
                    (parameterize ([current-indent (make-indent 2)])
                      (render-flow (car flows) part ht #t)))
             (for/list ([d (in-list (cdr flows))])
               (indented-newline)
               (printf "* ")
               (parameterize ([current-indent (make-indent 2)])
                 (render-flow d part ht #f)))))))

    (define/override (render-paragraph p part ri)
      (define (write-note)
        (write-string (make-string (note-depth) #\>))
        (unless (zero? (note-depth))
          (write-string " ")))
      (define o (open-output-string))
      (parameterize ([current-output-port o])
        (super render-paragraph p part ri))
      ;; 1. Remove newlines so we can re-wrap the text.
      ;;
      ;; 2. Combine adjacent code spans into one. These result from
      ;; something like @racket[(x y)] being treated as multiple
      ;; RktXXX items rather than one. (Although it would be
      ;; more-correct to handle them at that level, I don't easily see
      ;; how. As a result I'm handling it after-the-fact, at the
      ;; text/Markdown stage.)
      (define to-wrap (regexp-replaces (get-output-string o)
                                       '([#rx"\n" " "]   ;1
                                         [#rx"``" ""]))) ;2
      (define lines (wrap-line (string-trim to-wrap) (- 72 (current-indent))))
      (write-note)
      (write-string (car lines))
      (for ([line (in-list (cdr lines))])
        (newline) (indent) (write-note) (write-string line))
      (newline)
      null)

    (define/private (content-style e)
      (cond
       [(element? e) (element-style e)]
       [(multiarg-element? e) (multiarg-element-style e)]
       [else #f]))

    (define in-bold? (make-parameter #f))
    (define in-italic? (make-parameter #f))
    (define in-code? (make-parameter #f))
    (define in-link? (make-parameter #f))
    (define preserving-spaces? (make-parameter #f))

    (define (bold? i)
      (and (element? i) (eq? (element-style i) 'bold)))

    (define (italic? i)
      (and (element? i) (eq? (element-style i) 'italic)))

    (define (code? i)
      (and (element? i)
           (let ([s (element-style i)])
             (or (eq? 'tt s)
                 (and (style? s)
                      (style-name s)
                      (regexp-match? #rx"^Rkt[A-Z]" (style-name s)))))))

    (define (link? i)
      (let ([s (content-style i)])
        (and (style? s) (findf target-url? (style-properties s)))))

    (define (link-from i)
      (target-url-addr (findf target-url? (style-properties (content-style i)))))

    (define (preserve-spaces? i)
      (and (element? i)
           (let ([s (element-style i)])
             (or (eq? 'hspace s)
                 (and (style? s)
                      (eq? 'hspace (style-name s)))))))

    (define (sanitize-parens str)
      (regexp-replace #rx"[\\(\\)]" str "\\&"))

    (define/override (render-content i part ri)
      (define (recurse-wrapped str param)
        (display str)
        (begin0
          (parameterize ([param #t])
            (render-content i part ri))
          (display str)))

      (cond
        [(and (code? i) (not (in-code?)))
          (recurse-wrapped "`" in-code?)]

        [(and (bold? i) (not (in-bold?)))
          (recurse-wrapped "**" in-bold?)]

        [(and (italic? i) (not (in-italic?)))
          (recurse-wrapped "_" in-italic?)]

        [(and (preserve-spaces? i) (not (preserving-spaces?)))
          (parameterize ([preserving-spaces? #t])
            (render-content i part ri))]

        [(and (link? i) (not (in-link?)))
          (let ([link (link-from i)])
            (display "[")
            (begin0
              (parameterize ([in-link? #t])
                (render-content i part ri))
              (printf "](~a)" (sanitize-parens link))))]

        [else (super render-content i part ri)]))

    (define/override (render-nested-flow i part ri starting-item?)
      (define s (nested-flow-style i))
      (unless (memq 'decorative (style-properties s))
        (define note? (equal? (style-name s) "refcontent"))
        (when note?
          (note-depth (add1 (note-depth))))
        (begin0 (super render-nested-flow i part ri starting-item?)
          (when note?
            (note-depth (sub1 (note-depth)))))))

    (define/override (render-other i part ht)
      (cond
        [(symbol? i)
         (display (case i
                    [(mdash) "\U2014"]
                    [(ndash) "\U2013"]
                    [(ldquo) "\U201C"]
                    [(rdquo) "\U201D"]
                    [(lsquo) "\U2018"]
                    [(rsquo) "\U2019"]
                    [(lang) ">"]
                    [(rang) "<"]
                    [(rarr) "->"]
                    [(nbsp) "\uA0"]
                    [(prime) "'"]
                    [(alpha) "\u03B1"]
                    [(infin) "\u221E"]
                    [else (error 'markdown-render "unknown element symbol: ~e"
                                 i)]))]
        [(string? i)
         (let* ([i (if (in-code?)
                       (regexp-replace** i '([#rx"``" . "\U201C"]
                                             [#rx"''" . "\U201D"]))
                       (regexp-replace* #px"([#_*`\\[\\(\\]\\)]{1})" i "\\\\\\1"))]
                [i (if (preserving-spaces?)
                       (regexp-replace* #rx" " i "\uA0")
                       i)])
           (display i))]
        [else (write i)])
      null)

    (super-new)))

(define (regexp-replace** str ptns&reps)
  (for/fold ([str str])
            ([ptn (map car ptns&reps)]
             [rep (map cdr ptns&reps)])
    (regexp-replace* ptn str rep)))


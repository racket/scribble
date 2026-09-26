#lang racket/base

;; A deliberately small LaTeX reader for BibTeX's human-readable fields.
;; Grouping braces become visually transparent Scribble elements; their
;; private style property lets the BibTeX name parser recognize protected
;; words without teaching Autobib anything about BibTeX or LaTeX.
;; Supported: grouping, \emph{...}, \texttt{...}, common control symbols,
;; dotless i/j, and common accents. Declaration-style font switches
;; (\em, \tt, \bf) are intentionally not interpreted.

(require racket/match
         racket/list
         racket/string
         scribble/core
         scribble/base)

(provide latex->content bibtex-group?)

(define bibtex-group-style (make-style "BibtexGroup" '(bibtex-group)))
(define raw-tex-style (make-style #f '(exact-chars)))
(define bibtex-smallcaps-style (make-style "BibtexSmallCaps" null))
(define bibtex-inline-math-style
  (make-style "BibtexInlineMath" '(exact-chars)))
(define bibtex-display-math-style
  (make-style "BibtexDisplayMath" '(exact-chars)))

(define (bibtex-group? value)
  (and (element? value)
       (and (memq 'bibtex-group
                  (style-properties (element-style value)))
            #t)))

(define accents
  (hash #\' "\u0301" #\` "\u0300" #\" "\u0308"
        #\~ "\u0303" #\^ "\u0302" #\= "\u0304"
        #\c "\u0327" #\v "\u030c" #\H "\u030b"
        #\u "\u0306" #\k "\u0328" #\. "\u0307"
        #\r "\u030a"))

(define letter-commands
  (hash "o" "ø" "O" "Ø"
        "l" "ł" "L" "Ł"
        "ss" "ß"
        "aa" "å" "AA" "Å"
        "ae" "æ" "AE" "Æ"
        "oe" "œ" "OE" "Œ"
        "dh" "ð" "DH" "Ð"
        "th" "þ" "TH" "Þ"))

(define (latex->content source)
  (cond
    [source
     (define ip (open-input-string source))
     (define (read-word)
       (list->string (let loop ()
                       (define c (peek-char ip))
                       (if (and (char? c) (char-alphabetic? c))
                           (cons (read-char ip) (loop))
                           null))))
  
     (define (read-control-whitespace)
       (list->string (let spaces ()
                       (define c (peek-char ip))
                       (if (and (char? c) (char-whitespace? c))
                           (cons (read-char ip) (spaces))
                           null))))
  
     (define (read-url-argument)
       ;; The opening brace is still at the input port.
       (read-char ip)
       (define out (open-output-string))
       (let loop ([depth 1])
         (match (read-char ip)
           [(? eof-object?) (error 'latex->content "unclosed URL in ~e" source)]
           [#\\
            (define next (read-char ip))
            (when (eof-object? next)
              (error 'latex->content "trailing backslash in URL in ~e" source))
            (unless (memv next '(#\% #\& #\# #\_ #\~ #\{ #\} #\\))
              (write-char #\\ out))
            (write-char next out)
            (loop depth)]
           [#\{
            (write-char #\{ out)
            (loop (add1 depth))]
           [#\}
            (if (= depth 1)
                (get-output-string out)
                (begin
                  (write-char #\} out)
                  (loop (sub1 depth))))]
           [c
            (write-char c out)
            (loop depth)])))
  
     (define (normalize pieces)
       (match pieces
         ['() ""]
         [(list one) one]
         [_ pieces]))
  
     ;; For unknown commands, retain immediately attached braced
     ;; arguments literally, including nested braces and escapes.
     (define (read-raw-arguments)
       (define out (open-output-string))
       (let arguments ()
         (when (eqv? (peek-char ip) #\{)
           (let ([depth 0])
             (let copy ()
               (match (read-char ip)
                 [(? eof-object?) (error 'latex->content "unclosed argument in ~e" source)]
                 [#\\
                  (write-char #\\ out)
                  (define next (read-char ip))
                  (when (eof-object? next)
                    (error 'latex->content "trailing backslash in ~e" source))
                  (write-char next out)
                  (copy)]
                 [#\{
                  (set! depth (add1 depth))
                  (write-char #\{ out)
                  (copy)]
                 [#\}
                  (set! depth (sub1 depth))
                  (write-char #\} out)
                  (unless (zero? depth)
                    (copy))]
                 [c
                  (write-char c out)
                  (copy)]))
             (arguments))))
       (get-output-string out))
  
     (define (read-math)
       ;; The opening $ was already consumed by read-group.
       (define display? (eqv? (peek-char ip) #\$))
       (when display?
         (read-char ip))
       (define out (open-output-string))
       (display (if display? "$$" "$") out)
       (let loop ()
         (match (read-char ip)
           [(? eof-object?) (error 'latex->content "unclosed math in ~e" source)]
           [#\\
            (write-char #\\ out)
            (define next (read-char ip))
            (when (eof-object? next)
              (error 'latex->content "trailing backslash in math in ~e" source))
            (write-char next out)
            (loop)]
           [#\$
            (write-char #\$ out)
            (if (and display? (not (eqv? (peek-char ip) #\$)))
                (loop)
                (begin
                  (when display?
                    (write-char (read-char ip) out))
                  (get-output-string out)))]
           [c
            (write-char c out)
            (loop)])))
  
     (define (math-element raw)
       (make-element (if (string-prefix? raw "$$") bibtex-display-math-style bibtex-inline-math-style)
                     (list raw)))
  
     (define (read-group in-group?)
       (define pieces null)
       (define out (open-output-string))
  
       (define (flush!)
         (define s (get-output-string out))
         (unless (string=? s "")
           (set! pieces (cons s pieces)))
         (set! out (open-output-string)))
  
       (define (emit! value)
         (flush!)
         (set! pieces (cons value pieces)))
  
       (define (finish)
         (flush!)
         (reverse pieces))
  
       (define (read-accent-argument)
         (match (peek-char ip)
           [#\{
            (read-char ip)
            (content->string (read-group #t))]
           [#\\
            (read-char ip)
            (define name (read-word))
            (cond
              [(string=? name "i") "ı"]
              [(string=? name "j") "ȷ"]
              [else name])]
           [(? char? c) (string (read-char ip))]
           [_ (error 'latex->content "missing accent argument in ~e" source)]))
  
       (define (emit-accent! accent)
         (define argument (read-accent-argument))
         (when (string=? argument "")
           (error 'latex->content "empty accent argument in ~e" source))
         (emit! (let ([first-char (substring argument 0 1)])
                  (string-normalize-nfc (string-append (if (string=? first-char "ı") "i" first-char)
                                                       (hash-ref accents accent)
                                                       (substring argument 1))))))
  
       (let loop ()
         (match (read-char ip)
           [(? eof-object?)
            (when in-group?
              (error 'latex->content "unclosed brace in ~e" source))
            (finish)]
           [#\{
            (emit! (make-element bibtex-group-style (read-group #t)))
            (loop)]
           [#\}
            (unless in-group?
              (error 'latex->content "unexpected closing brace in ~e" source))
            (finish)]
           [#\$
            (emit! (math-element (read-math)))
            (loop)]
           [#\~
            (emit! 'nbsp)
            (loop)]
           [#\\
            (define next (peek-char ip))
            (cond
              [(eof-object? next) (error 'latex->content "trailing backslash in ~e" source)]
              [(char-alphabetic? next)
               (define word (read-word))
               (cond
                 [(member word '("emph" "texttt" "textit" "textbf" "textsc"))
                  (define whitespace (read-control-whitespace))
                  (if (eqv? (peek-char ip) #\{)
                      (let ([body (begin
                                    (read-char ip)
                                    (read-group #t))])
                        (emit! (cond
                                 [(string=? word "emph") (apply emph body)]
                                 [(string=? word "texttt") (apply tt body)]
                                 [(string=? word "textit") (apply italic body)]
                                 [(string=? word "textbf") (apply bold body)]
                                 [else (make-element bibtex-smallcaps-style body)])))
                      (emit! (make-element raw-tex-style
                                           (list (string-append "\\" word whitespace)))))]
                 [(string=? word "url")
                  (define whitespace (read-control-whitespace))
                  (if (eqv? (peek-char ip) #\{)
                      (emit! (url (read-url-argument)))
                      (emit! (make-element raw-tex-style (list (string-append "\\url" whitespace)))))]
                 [(and (= (string-length word) 1) (hash-has-key? accents (string-ref word 0)))
                  (emit-accent! (string-ref word 0))]
                 [(hash-has-key? letter-commands word)
                  (read-control-whitespace)
                  (display (hash-ref letter-commands word) out)]
                 [(string=? word "i")
                  (read-control-whitespace)
                  (display "ı" out)]
                 [(string=? word "j")
                  (read-control-whitespace)
                  (display "ȷ" out)]
                 [else
                  (emit! (make-element raw-tex-style
                                       (list (string-append "\\" word (read-raw-arguments)))))])
               (loop)]
              [else
               (define symbol (read-char ip))
               (cond
                 [(hash-has-key? accents symbol) (emit-accent! symbol)]
                 [(memv symbol '(#\{ #\} #\% #\& #\$ #\# #\_)) (write-char symbol out)]
                 [(char=? symbol #\space) (write-char #\space out)]
                 ;; Unknown control symbol, including \\, remains
                 ;; literal in LaTeX rather than silently vanishing.
                 [else (emit! (make-element raw-tex-style (list (string #\\ symbol))))])
               (loop)])]
           [c
            (write-char c out)
            (loop)])))
  
     (normalize (read-group #f))]
    [else #f]))

(module+ test
  (require rackunit)
  (check-equal? (content->string (latex->content "Guy L. {Steele Jr.}"))
                "Guy L. Steele Jr.")
  (check-true (bibtex-group? (latex->content "{Steele Jr.}")))
  (check-equal? (content->string (latex->content "\\emph{A \\texttt{B}}"))
                "A B")
  (check-equal? (content->string (latex->content "\\{x\\} \\% \\&"))
                "{x} % &")
  (check-equal? (content->string (latex->content "J.~of Things"))
                "J.\u00a0of Things")
  (check-equal? (content->string (latex->content "The $\\lambda$-calculus"))
                "The $\\lambda$-calculus")
  (check-equal? (content->string (latex->content "The $$x^2$$ formula"))
                "The $$x^2$$ formula")
  (check-exn #rx"unclosed math"
             (lambda () (latex->content "The $x+y")))
  (check-equal?
   (content->string
    (latex->content "Fran\\c{c}ois Erd\\H{o}s \\v{S}ediv \\u{g} \\k{a} \\.{z} \\r{a}"))
   "François Erdős Šediv ğ ą ż å")
  (check-equal?
   (content->string
    (latex->content "S{\\o}ren {\\L}ukasz D{\\ae}dalus Fu{\\ss}, \\O{} \\AA{} \\oe{}"))
   "Søren Łukasz Dædalus Fuß, Ø Å œ")
  (check-equal?
   (content->string (latex->content "V\\'{\\i}ctor and V\\'ictor"))
   "Víctor and Víctor")
  (check-equal?
   (content->string (latex->content "\\i \\j"))
   "ı ȷ")
  (check-equal?
   (content->string
    (latex->content "\\textit{A \\textbf{B}} \\textsc{C} \\emph{D}"))
   "A B C D")
  (check-equal?
   (style-name (element-style (latex->content "\\textsc{SmallCaps}")))
   "BibtexSmallCaps")
  (check-equal?
   (content->string
    (latex->content "\\url{https://example.org/~alice/a_b?x=1&y=2}"))
   "https://example.org/~alice/a_b?x=1&y=2")
  (check-equal?
   (content->string
    (latex->content "\\url{https://example.org/a\\_b\\%20c}"))
   "https://example.org/a_b%20c")
  (check-exn #rx"unclosed URL"
             (lambda () (latex->content "\\url{https://example.org")))

  (check-equal?
   (style-name (element-style (latex->content "$\\lambda$")))
   "BibtexInlineMath")
  (check-equal?
   (style-name (element-style (latex->content "$$x^2$$")))
   "BibtexDisplayMath")
  (check-equal?
   (content->string (latex->content "Price \\$5; $\\lambda$"))
   "Price $5; $\\lambda$"))

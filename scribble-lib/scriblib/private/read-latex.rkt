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

(define bibtex-group-style (make-style #f '(bibtex-group)))
(define raw-tex-style (make-style #f '(exact-chars)))

(define (bibtex-group? value)
  (and (element? value)
       (and (memq 'bibtex-group
                  (style-properties (element-style value)))
            #t)))

(define accents
  (hash #\' "\u0301" #\` "\u0300" #\" "\u0308"
        #\~ "\u0303" #\^ "\u0302" #\= "\u0304"))

(define (latex->content source)
  (and source
       (let ([ip (open-input-string source)])
         (define (read-word)
           (list->string
            (let loop ()
              (define c (peek-char ip))
              (if (and (char? c) (char-alphabetic? c))
                  (cons (read-char ip) (loop))
                  null))))

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
                   [(? eof-object?)
                    (error 'latex->content "unclosed argument in ~e" source)]
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
                    (unless (zero? depth) (copy))]
                   [c (write-char c out) (copy)]))
                 (arguments))))
           (get-output-string out))

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
                (cond [(string=? name "i") "ı"]
                      [(string=? name "j") "ȷ"]
                      [else name])]
               [(? char? c) (string (read-char ip))]
               [_ (error 'latex->content "missing accent argument in ~e" source)]))

           (define (emit-accent! accent)
             (define argument (read-accent-argument))
             (when (string=? argument "")
               (error 'latex->content "empty accent argument in ~e" source))
             (emit! (string-normalize-nfc
                     (string-append (substring argument 0 1)
                                    (hash-ref accents accent)
                                    (substring argument 1)))))

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
               [#\\
                (define next (peek-char ip))
                (cond
                  [(eof-object? next)
                   (error 'latex->content "trailing backslash in ~e" source)]
                  [(char-alphabetic? next)
                   (define word (read-word))
                   (cond
                     [(member word '("emph" "texttt"))
                      ;; Spaces after a control word are ignored by TeX.
                      (define whitespace
                        (list->string
                         (let spaces ()
                           (define c (peek-char ip))
                           (if (and (char? c) (char-whitespace? c))
                               (cons (read-char ip) (spaces))
                               null))))
                      (if (eqv? (peek-char ip) #\{)
                          (let ([body (begin
                                        (read-char ip)
                                        (read-group #t))])
                            (emit! (if (string=? word "emph")
                                       (apply emph body)
                                       (apply tt body))))
                          (emit! (make-element raw-tex-style
                                               (list (string-append "\\" word whitespace)))))]
                     [(string=? word "i") (display "ı" out)]
                     [(string=? word "j") (display "ȷ" out)]
                     [else
                      (emit! (make-element
                              raw-tex-style
                              (list (string-append "\\" word
                                                   (read-raw-arguments)))))])
                   (loop)]
                  [else
                   (define symbol (read-char ip))
                   (cond
                     [(hash-has-key? accents symbol)
                      (emit-accent! symbol)]
                     [(memv symbol '(#\{ #\} #\% #\& #\$ #\# #\_))
                      (write-char symbol out)]
                     [(char=? symbol #\space)
                      (write-char #\space out)]
                     [else
                      ;; Unknown control symbol, including \\, remains
                      ;; literal in LaTeX rather than silently vanishing.
                      (emit! (make-element raw-tex-style
                                           (list (string #\\ symbol))))])
                   (loop)])]
               [c (write-char c out) (loop)])))

         (normalize (read-group #f)))))

(module+ test
  (require rackunit)
  (check-equal? (content->string (latex->content "Guy L. {Steele Jr.}"))
                "Guy L. Steele Jr.")
  (check-true (bibtex-group? (latex->content "{Steele Jr.}")))
  (check-equal? (content->string (latex->content "\\emph{A \\texttt{B}}"))
                "A B")
  (check-equal? (content->string (latex->content "\\{x\\} \\% \\&"))
                "{x} % &")
  (check-equal? (content->string (latex->content "V\\'{\\i}ctor"))
                "Vı́ctor"))

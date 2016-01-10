#lang scribble/base
@(require scribble/core)

@(define P (make-numberer (lambda (v parent-number)
                            (values (list (format "[~a]" v) ; number in brackets
                                          "")  ; no separator afterward
                                    (add1 v))) ; increment section number
                          1)) @; count from 1
@(define PL (make-numberer (lambda (v parent-number)
                             (values (list (if (null? parent-number)
                                               (string v) ; top-level section is uppercase
                                               (string-downcase (string v))) ; nested is lowercase
                                           ",")         ;  "," as separator
                                     (integer->char (add1 (char->integer v))))) ; increment letter
                           #\A)) @; count from A

@(define (P-section . s)  (section #:style (style #f (list P))  s))
@(define (PL-section . s) (section #:style (style #f (list PL)) s))
@(define (PL-subsection . s) (subsection #:style (style #f (list PL)) s))

@title{Two Tracks}

@P-section{Px}

@PL-section{Py}

@PL-section{PLx}

@P-section{Pz}
@PL-subsection{PL-subx}
@subsection{Normal}
@PL-subsection{PL-suby}

@PL-section{PLy}

@PL-section{PLz}
@subsection{Normal2}

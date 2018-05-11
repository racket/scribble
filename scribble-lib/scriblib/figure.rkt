#lang scheme/base
(require racket/contract/base
         scribble/manual
         scribble/core
         scribble/decode
         scribble/html-properties
         scribble/latex-properties
         setup/main-collects
         "private/counter.rkt"
         scribble/private/lang-parameters)

(provide figure
         figure*
         figure**
         figure-here
         (contract-out
          [Figure-target (->* (string?)
                              (#:continue? any/c)
                              element?)]
          [Figure-ref (->* (string?)
                           (#:link-render-style link-render-style?)
                           #:rest (listof string?)
                           element?)]
          [figure-ref (->* (string?)
                           (#:link-render-style link-render-style?)
                           #:rest (listof string?)
                           element?)])
         left-figure-style
         center-figure-style
         right-figure-style
         suppress-floats
         (rename-out [left-figure-style left]))

(define figure-style-extras
  (let ([abs (lambda (s)
               (path->main-collects-relative
                (collection-file-path s "scriblib")))])
    (list 'never-indents
          (make-css-addition (abs "figure.css"))
          (make-tex-addition (abs "figure.tex")))))

;; outer layer:
(define herefigure-style  (make-style "Herefigure" figure-style-extras))
(define figure-style (make-style "Figure" figure-style-extras))
(define figuremulti-style (make-style "FigureMulti" figure-style-extras))
(define figuremultiwide-style (make-style "FigureMultiWide" figure-style-extras))

;; middle layer:
(define center-figure-style (make-style "Centerfigure" figure-style-extras))
(define left-figure-style (make-style "Leftfigure" figure-style-extras))
(define right-figure-style (make-style "Rightfigure" figure-style-extras))

;; inner layer:
(define figureinside-style (make-style "FigureInside" figure-style-extras))

(define legend-style (make-style "Legend" figure-style-extras))
(define legend-continued-style (make-style "LegendContinued" figure-style-extras))

(define centertext-style (make-style "Centertext" figure-style-extras))

;; See "figure.js":
(define figure-target-style
  (make-style #f
              (list
               (make-attributes '((x-target-lift . "Figure")))
               (make-js-addition
                (path->main-collects-relative
                 (collection-file-path "figure.js" "scriblib"))))))

(define (make-figure-ref c s)
  (element (style "FigureRef" (list* (command-extras (list s))
                                     figure-style-extras))
    c))
(define (make-figure-target c s)
  (element (style "FigureTarget" (cons (command-extras (list s))
                                       figure-style-extras))
    c))

(define (figure tag caption 
                #:style [style center-figure-style]
                #:label-sep [label-sep (default-figure-label-sep)]
                #:label-style [label-style #f]
                #:continue? [continue? #f]
                . content)
  (figure-helper figure-style style label-sep label-style tag caption content continue?))

(define (figure-here tag caption 
                     #:style [style center-figure-style]
                     #:label-sep [label-sep (default-figure-label-sep)]
                     #:label-style [label-style #f]
                     #:continue? [continue? #f]
                     . content)
  (figure-helper herefigure-style style label-sep label-style tag caption content continue?))

(define (figure* tag caption 
                 #:style [style center-figure-style]
                 #:label-sep [label-sep (default-figure-label-sep)]
                 #:label-style [label-style #f]
                 #:continue? [continue? #f]
                 . content)
  (figure-helper figuremulti-style style label-sep label-style tag caption content continue?))
(define (figure** tag caption 
                  #:style [style center-figure-style]
                  #:label-sep [label-sep (default-figure-label-sep)]
                  #:label-style [label-style #f]
                  #:continue? [continue? #f]
                  . content)
  (figure-helper figuremultiwide-style style label-sep label-style tag caption content continue?))

(define (figure-helper figure-style content-style label-sep label-style tag caption content continue?)
  (make-nested-flow 
   figure-style 
   (list
    (make-nested-flow
     content-style
     (list (make-nested-flow figureinside-style (decode-flow content))))
    (make-paragraph
     centertext-style
     (list (make-element (if continue?
                             legend-continued-style
                             legend-style)
                         (list (Figure-target tag
                                              #:label-sep label-sep
                                              #:label-style label-style
                                              #:continue? continue?)
                               (make-element (default-figure-caption-style) caption))))))))

(define figures (new-counter "figure" 
                             #:target-wrap make-figure-target
                             #:ref-wrap make-figure-ref))
(define (Figure-target tag
                       #:continue? [continue? #f]
                       #:label-sep [label-sep ": "]
                       #:label-style [label-style #f])
  (counter-target figures tag 
                  (default-figure-label-text)
                  #:label-suffix (list (if continue? " (continued)" "") label-sep)
                  #:label-style label-style
                  #:target-style figure-target-style
                  #:continue? continue?))

(define (ref-proc initial)
  (lambda (tag #:link-render-style [link-style #f]
               . tags)
    (cond
      [(null? tags)
       (make-element
        #f
        (counter-ref figures tag (string-append initial "igure")
                     #:link-render-style link-style))]
      [(null? (cdr tags))
       (define tag1 tag)
       (define tag2 (car tags))
       (make-element #f (list (counter-ref figures tag1 (string-append initial "igures")
                                           #:link-render-style link-style)
                              " and "
                              (counter-ref figures tag2 #f
                                           #:link-render-style link-style)))]
      [else
       (make-element #f (cons (counter-ref figures tag (string-append initial "igures")
                                           #:link-render-style link-style)
                              (let loop ([tags tags])
                                (cond
                                  [(null? (cdr tags))
                                   (list ", and "
                                         (counter-ref figures (car tags) #f
                                                      #:link-render-style link-style))]
                                  [else
                                   (list* ", "
                                          (counter-ref figures (car tags) #f
                                                       #:link-render-style link-style)
                                          (loop (cdr tags)))]))))])))

(define Figure-ref (ref-proc "F"))
(define figure-ref (ref-proc "f"))

(define (suppress-floats)
  (make-element "suppressfloats" null))

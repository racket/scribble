#lang racket/base
(require scribble/core
         scribble/decode
         scribble/private/lang-parameters)

(provide new-counter
         counter-target
         counter-ref
         counter-collect-value)

(define-struct counter ([n #:mutable] name target-wrap ref-wrap))

(define (new-counter name
                     #:target-wrap [target-wrap (lambda (c s) c)]
                     #:ref-wrap [ref-wrap (lambda (c s) c)])
  (make-counter 0 name target-wrap ref-wrap))

(define (tag->counter-tag counter tag . kind)
  (if (generated-tag? tag)
      `(,(string->symbol (format "counter-~a" kind)) ,tag)
      `(counter (,(counter-name counter) ,tag ,@kind))))

(define (counter-target counter tag label
                        #:target-style [target-style #f]
                        #:label-style [label-style #f]
                        #:label-suffix [label-suffix '()]
                        #:continue? [continue? #f]
                        . content)
  (let ([content (decode-content content)])
    (define c
      (make-target-element
       target-style
       (list
        (make-collect-element
         #f
         (list
          (make-delayed-element
           (lambda (renderer part ri)
             (let ([n (resolve-get part ri (tag->counter-tag counter tag "value"))])
               (cons
                (make-element label-style
                              (let ([l (cons (make-element (default-figure-counter-style) (format "~a" n))
                                             (decode-content (list label-suffix)))])
                                (if label
                                    (list* label 'nbsp l)
                                    l)))
                content)))
           (lambda () (if label
                          (list* label 'nbsp "N" content)
                          (cons "N" content)))
           (lambda () (if label
                          (list* label 'nbsp "N" content)
                          (cons "N" content)))))
         (lambda (ci)
           (let ([n (if continue?
                        (counter-n counter)
                        (add1 (counter-n counter)))])
             (set-counter-n! counter n)
             (collect-put! ci (generate-tag (tag->counter-tag counter tag "value") ci) n)))))
       (tag->counter-tag counter tag)))
    (if (counter-target-wrap counter)
        ((counter-target-wrap counter)
         c
         ;; Don't use this argument:
         (format "t:~a" (t-encode (tag->counter-tag counter tag))))
        c)))

;; The use of this function is a leftover for backward compatibility.
;; Duplicating the linking functionality of `link-element`, etc., is
;; a bad idea.
(define (t-encode s)
  (apply
   string-append
   (map (lambda (c)
          (cond
            [(and (or (char-alphabetic? c) (char-numeric? c))
                  ((char->integer c) . < . 128))
             (string c)]
            [(char=? c #\space) "_"]
            [else (format "x~x" (char->integer c))]))
        (string->list (format "~s" s)))))

(define (counter-ref counter tag label
                     #:link-render-style [link-style #f])
  (make-delayed-element
   (lambda (renderer part ri)
     (let ([n (resolve-get part ri (tag->counter-tag counter tag "value"))])
       (let ([n (if (counter-ref-wrap counter)
                    ((counter-ref-wrap counter)
                     (format "~a" n)
                     ;; Don't use this argument:
                     (format "t:~a" (t-encode (list 'counter (list (counter-name counter) tag)))))
                    (list (format "~a" n)))]
             [link-number-only? (eq? (link-render-style-mode
                                      (or link-style
                                          (current-link-render-style)))
                                     'number)])
         (cond
           [(and label link-number-only?)
            (make-element #f
                          (list label 'nbsp
                                (make-link-element
                                 #f
                                 (list n)
                                 (tag->counter-tag counter tag))))]
           [else
            (make-link-element
             #f
             (if label
                 (list label 'nbsp n)
                 n)
             (tag->counter-tag counter tag))]))))
   (lambda () (if label
                  (list label 'nbsp "N")
                  (list "N")))
   (lambda () (if label
                  (list label 'nbsp "N")
                  (list "N")))))

(define (counter-collect-value counter)
  (counter-n counter))

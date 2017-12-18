#lang racket/base
(require "../core.rkt"
         "../html-properties.rkt")

(provide same-index-entry?
         extract-index-link-targets)

(define (same-index-entry? a-blocks b-blocks)
  (and (= (length a-blocks) (length b-blocks))
       ;; We expect an index entry to have a single paragraph, but
       ;; allow a list:
       (for/and ([a (in-list a-blocks)]
                 [b (in-list b-blocks)])
         (and (paragraph? a)
              (paragraph? b)
              ;; Compare paragraph content, paying attention to style,
              ;; but not paying attention to link targets:
              (let loop ([a (paragraph-content a)]
                         [b (paragraph-content b)])
                (cond
                  [(equal? a b) #t]
                  [(alpha-anchor-content a) => (lambda (a) (loop a b))]
                  [(alpha-anchor-content b) => (lambda (b) (loop a b))]
                  [(and (pair? a) (pair? b))
                   (and (loop (car a) (car b))
                        (loop (cdr a) (cdr b)))]
                  [(and (element? a)
                        (element? b))
                   (and (equal? (element-content a)
                                (element-content b))
                        (equal? (element-style a)
                                (element-style b)))]
                  [else #f]))))))

(define (alpha-anchor-content e)
  (and (element? e)
       (let ([s (element-style e)])
         (and s
              (style? s)
              (not (style-name s))
              (= 1 (length (style-properties s)))
              (url-anchor? (car (style-properties s)))))
       (let ([c (element-content e)])
         (cond
           [(and (pair? c) (null? (cdr c))) (car c)]
           [else c]))))

(define (extract-index-link-targets blockss)
  (apply
   append
   (for*/list ([blocks (in-list blockss)]
               [b (in-list blocks)])
     (cond
       [(paragraph? b)
        (let content-loop ([c (paragraph-content b)])
          (cond
            [(null? c) null]
            [(pair? c) (append (content-loop (car c))
                               (content-loop (cdr c)))]
            [(link-element? c) (list c)]
            [else null]))]
       [else null]))))

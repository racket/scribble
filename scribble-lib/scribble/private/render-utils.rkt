#lang racket/base
(require "../core.rkt")

(provide part-style?
         part-tag-prefix-string
         select-suffix
         extract-table-cell-styles
         empty-content?
         extract-cell-paddingss
         extract-column-paddings)

(define (part-style? p s)
  (memq s (style-properties (part-style p))))

(define (select-suffix path suggested-suffixes accepted-suffixes)
  (or (for/or ([suggested (in-list suggested-suffixes)])
        (and (member suggested accepted-suffixes)
             (let ([p (bytes->path 
                       (bytes-append (path->bytes (if (string? path)
                                                      (string->path path)
                                                      path))
                                     (string->bytes/utf-8 suggested)))])
               (and (file-exists? p)
                    p))))
      path))

(define (extract-table-cell-styles t)
  (define vars (style-properties (table-style t)))
  (or (let ([l (for/or ([v (in-list vars)])
                 (and (table-cells? v)
                      (table-cells-styless v)))])
        (and l
             (unless (= (length l) (length (table-blockss t)))
               (error 'table 
                      "table-cells property list's length does not match row count: ~e vs. ~e"
                      l (length (table-blockss t))))
             (for-each (lambda (l row)
                         (unless (= (length l) (length row))
                           (error 'table
                                  "table-cells property list contains a row whose length does not match the content: ~e vs. ~e"
                                  l (length row))))
                       l (table-blockss t))
             l))
      (let ([cols (ormap (lambda (v) (and (table-columns? v) v)) vars)])
        (and cols
             (let ([cols (table-columns-styles cols)])
               (map (lambda (row)
                      (unless (= (length cols) (length row))
                        (error 'table
                               "table-columns property list's length does not match a row length: ~e vs. ~e"
                               cols (length row)))
                      cols)
                    (table-blockss t)))))
      (map (lambda (row) (map (lambda (c) plain) row)) (table-blockss t))))

(define (empty-content? c) (null? c))

(define (part-tag-prefix-string d)
  (define p (part-tag-prefix d))
  (cond
    [(string? p) p]
    [(hash? p)
     (define s (hash-ref p 'tag-prefix #f))
     (and (string? s) s)]
    [else #f]))

(define (extract-cell-paddingss cell-styless)
  (for/list ([cell-styles (in-list cell-styless)])
    (for/list ([cell-style (in-list cell-styles)])
      (define pad (findf cell-padding-property? (style-properties cell-style)))
      (or pad
          (cell-padding-property 0 0 0 0)))))

;; extract common padding, if any, for all rows in each column;
;; result is `#f` in a column that doesn't have consistent padding
(define (extract-column-paddings cell-paddingss)
  (cond
    [(null? cell-paddingss)
     null]
    [else
     (let loop ([cell-paddingss cell-paddingss])
       (cond
         [(null? (car cell-paddingss)) null]
         [else
          (define column (map car cell-paddingss))
          (define next (map cdr cell-paddingss))
          (cons (let loop ([padding (car column)] [column (cdr column)])
                  (cond
                    [(null? column) padding]
                    [(and (= (cell-padding-property-left padding)
                             (cell-padding-property-left (car column)))
                          (= (cell-padding-property-right padding)
                             (cell-padding-property-right (car column))))
                     (loop padding (cdr column))]
                    [else #f]))
                (loop next))]))]))

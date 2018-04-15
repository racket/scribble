(module bnf racket
  (require scribble/decode
           (except-in scribble/struct 
                      element?)
           (only-in scribble/core
                    content?
                    element?
                    make-style
                    make-table-columns)
           )

  (provide (contract-out
            [BNF (-> (cons/c (or/c block? content?)
                             (non-empty-listof (or/c block? content?)))
                     ... 
                     table?)]
            [BNF-etc element?]
            ;; operate on content
            [BNF-seq (-> content? ... 
                         (or/c element? ""))]
            [BNF-seq-lines (-> (listof content?) ... 
                               block?)]
            [BNF-alt (-> content? ...
                         element?)]
            [BNF-alt/close (-> content? ...
                               element?)]
            ;; operate on pre-content
            [BNF-group (-> pre-content? ...
                           element?)]
            [nonterm (-> pre-content? ...
                         element?)]
            [optional (-> pre-content? ...
                          element?)]
            [kleenestar (-> pre-content? ...
                            element?)]
            [kleeneplus (-> pre-content? ...
                            element?)]
            [kleenerange (-> any/c any/c pre-content? ...
                             element?)]
            ))
  
  
  (define spacer (make-element 'hspace (list " ")))
  (define equals (make-element 'tt (list spacer "::=" spacer)))
  (define alt (make-element 'tt (list spacer spacer "|" spacer spacer)))

  (define (as-flow i) (make-flow (list (if (block? i)
                                           i
                                           (make-paragraph (list i))))))


  (define baseline (make-style #f '(baseline)))

  (define (BNF . defns)
    (make-table
     (make-style #f
                 (list
                  (make-table-columns
                   (list baseline baseline baseline baseline))))
     (apply
      append
      (map (match-lambda
             [(cons lhs (cons rhs0 more-rhs))
              (cons
               (list (as-flow spacer) (as-flow lhs) (as-flow equals) (as-flow rhs0))
               (map (lambda (i)
                      (list (as-flow spacer) (as-flow " ") (as-flow alt) (as-flow i)))
                    more-rhs))])
           defns))))

  ;; interleave : (listof content?) element? -> element?
  (define (interleave l spacer)
    (make-element #f (cons (car l)
                           (apply append
                                  (map (lambda (i)
                                         (list spacer i))
                                       (cdr l))))))

  (define (BNF-seq . l)
    (if (null? l)
        ""
        (interleave l spacer)))

  (define (BNF-seq-lines . l)
    (make-table #f (map (lambda (row) (list (as-flow (apply BNF-seq row))))
                        l)))

  (define (BNF-alt . l)
    (interleave l alt))

  (define (BNF-alt/close . l)
    (interleave l (make-element 'roman " | ")))

  (define BNF-etc (make-element 'roman "..."))

  (define (nonterm . s)
    (make-element 'roman (append (list 'lang)
                                 (list (make-element 'italic (decode-content s)))
                                 (list 'rang))))

  (define (optional . s)
    (make-element #f (append (list (make-element 'roman "["))
                             (decode-content s)
                             (list (make-element 'roman "]")))))

  (define (BNF-group . s)
    (make-element #f (append (list (make-element 'roman "{"))
                             (list (apply BNF-seq (decode-content s)))
                             (list (make-element 'roman "}")))))

  (define (kleenestar . s)
    (make-element #f (append (decode-content s) (list (make-element 'roman "*")))))

  (define (kleeneplus . s)
    (make-element #f (append (decode-content s) (list (make-element 'superscript (list "+"))))))

  (define (kleenerange a b . s)
    (make-element #f (append (decode-content s) 
                             (list (make-element 'roman 
                                                 (make-element 'superscript 
                                                               (list (format "{~a,~a}" a b)))))))))

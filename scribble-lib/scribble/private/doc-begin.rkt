#lang racket/base
(require "../decode.rkt"
         (for-syntax racket/base
                     syntax/kerncase))

(provide doc-begin)

(define-syntax (doc-begin stx)
  (syntax-case stx ()
    [(_ m-id post-process exprs)
     #`(begin
         (define m-id (post-process (decode (list . #,(reverse (syntax->list #'exprs))))))
         (provide m-id))]
    [(_ m-id post-process exprs . body)
     ;; `body' probably starts with lots of string constants; it's
     ;; slow to trampoline on every string, so do them in a batch
     ;; here:
     (let loop ([body #'body]
                [accum null])
       (syntax-case body ()
         [(s . rest)
          (string? (syntax-e #'s))
          (loop #'rest (cons #'s accum))]
         [()
          (with-syntax ([(accum ...) accum])
            #`(doc-begin m-id post-process (accum ... . exprs)))]
         [(body1 . body)
          (with-syntax ([exprs (append accum #'exprs)])
            (let ([expanded (local-expand
                             #'body1 'module
                             (append (kernel-form-identifier-list)
                                     (syntax->list #'(provide
                                                      require))))])
              (syntax-case expanded (begin)
                [(begin body1 ...)
                 #`(doc-begin m-id post-process exprs body1 ... . body)]
                [(id . rest)
                 (and (identifier? #'id)
                      (ormap (lambda (kw) (free-identifier=? #'id kw))
                             (syntax->list #'(require
                                              provide
                                              define-values
                                              define-syntaxes
                                              begin-for-syntax
                                              module
                                              module*
                                              #%require
                                              #%provide
                                              #%declare))))
                 #`(begin #,expanded (doc-begin m-id post-process exprs . body))]
                [_else
                 #`(doc-begin m-id post-process 
                              ((pre-part #,expanded body1) . exprs) 
                              . body)])))]))]))

(define-syntax (pre-part stx)
  (syntax-case stx ()
    [(_ s e)
     (if (string? (syntax-case #'s (quote #%expression)
                    [(quote s) (syntax-e #'s)]
                    [(#%expression (quote s)) (syntax-e #'s)]
                    [(#%expression s) (syntax-e #'s)]
                    [_ (syntax-e #'s)]))
         #'s
         (with-syntax ([loc (datum->syntax #f 'loc #'e)])
           #'(check-pre-part s (quote-syntax loc))))]))

(define (check-pre-part v loc-stx)
  (unless (pre-part? v)
    (error
     (format
      "~a: not valid in document body (need a pre-part for decode) in: ~e"
      (cond
        [(and (syntax-source loc-stx) (syntax-line loc-stx))
         (format "~a:~a:~a" (syntax-source loc-stx) (syntax-line loc-stx) (syntax-column loc-stx))]
        [(and (syntax-source loc-stx) (syntax-position loc-stx))
         (format "~a:::~a" (syntax-source loc-stx) (syntax-position loc-stx))]
        [else 'document])
      v)))
  v)

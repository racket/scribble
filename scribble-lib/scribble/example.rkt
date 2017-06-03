#lang racket/base

(require "eval.rkt"
         racket/contract
         (only-in "struct.rkt" make-paragraph)
         (for-syntax racket/base
                     syntax/parse))

(define lang-option/c
  (or/c module-path? (list/c 'special symbol?) (cons/c 'begin list?)))

(define eval-factory/c
  (->* [(listof module-path?)] [#:pretty-print? any/c #:lang lang-option/c] any))

(provide examples

         ;; Re-exports:
         (contract-out
           [make-base-eval
            (->* [] [#:pretty-print? any/c #:lang lang-option/c] #:rest any/c any)]
           [make-base-eval-factory
            eval-factory/c]
           [make-eval-factory
            eval-factory/c]
           [close-eval
            (-> any/c any)]

           [scribble-exn->string
            (-> any/c string?)]
           [scribble-eval-handler
            (parameter/c (-> (-> any/c any) boolean? any/c any))]
           [make-log-based-eval
            (-> path-string? (or/c 'record 'replay) any)]))

(define example-title
  (make-paragraph (list "Example:")))
(define examples-title
  (make-paragraph (list "Examples:")))

(define-syntax (examples stx)
  (syntax-parse stx
    [(_ (~or (~optional (~seq #:eval eval:expr))
             (~optional (~and #:once once-kw))
             (~optional (~seq #:escape escape:id))
             (~optional (~seq #:label title:expr))
             (~optional (~and #:no-inset no-inset-kw))
             (~optional (~and #:no-prompt no-prompt-kw))
             (~optional (~and #:result-only no-form-kw))
             (~optional (~and #:no-result block-kw))
             (~optional (~and #:hidden no-result-kw))
             (~optional (~and #:preserve-source-locations preserve-srclocs-kw))
             (~optional (~seq #:lang module-name)))
        ...
        form:expr ...)
     (define once? (or (attribute once-kw)
                       (not (attribute eval))))
     (define eval-stx (or (attribute eval) #'(make-base-eval)))
     (define base-form
       (with-syntax ([eval (if once? #'once-eval eval-stx)]
                     [escape (or (attribute escape) #'unsyntax)])
         (cond
          [(attribute block-kw)
           (when (attribute module-name)
             (raise-syntax-error #f "#:block and #:module are mutually exclusive" stx))
           (cond
            [(attribute no-inset-kw)
             (syntax/loc stx
               (racketblock0+eval #:eval eval #:escape escape
                                  form ...))]
            [else
             (syntax/loc stx
               (racketblock+eval #:eval eval #:escape escape
                                 form ...))])]
          [(attribute module-name)
           (syntax/loc stx
             (racketmod+eval #:eval eval #:escape escape module-name
                             form ...))]
          [(attribute no-result-kw)
           (syntax/loc stx
             (interaction-eval #:eval eval form ...))]
          [(attribute no-form-kw)
           (syntax/loc stx
             (interaction-eval-show #:eval eval form ...))]
          [(attribute no-prompt-kw)
           (syntax/loc stx
             (interaction/no-prompt #:eval eval #:escape escape #:no-errors? #t
                                    form ...))]
          [(attribute no-inset-kw)
           (syntax/loc stx
             (interaction0 #:eval eval #:escape escape  #:no-errors? #t
                           form ...))]
          [else
           (syntax/loc stx
             (interaction #:eval eval #:escape escape  #:no-errors? #t
                          form ...))])))
     (define srcloced-form
       (cond
        [(attribute preserve-srclocs-kw)
         (with-syntax ([base-form base-form])
           (syntax/loc stx
             (with-eval-preserve-source-locations base-form)))]
        [else base-form]))
     (define examples-form
       (cond
        [(or (attribute title)
             (not (or (attribute block-kw)
                      (attribute module-name)
                      (attribute no-result-kw)
                      (attribute no-form-kw))))
         (with-syntax ([srcloced-form srcloced-form]
                       [title (or (attribute title)
                                  (cond
                                   [(= 1 (length (syntax->list #'(form ...))))
                                    #'example-title]
                                   [else #'examples-title]))])
           (syntax/loc stx (as-examples title srcloced-form)))]
        [else
         srcloced-form]))
     (if once?
         (with-syntax ([eval eval-stx]
                       [examples-form examples-form])
           (syntax/loc stx
             (let ([once-eval eval])
               (begin0
                examples-form
                (close-eval eval)))))
         examples-form)]))

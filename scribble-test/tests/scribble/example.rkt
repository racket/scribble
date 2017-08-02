#lang racket/base
(require rackunit scribble/example setup/path-to-relative
         (only-in racket/contract exn:fail:contract:blame?))

(test-case "scribble/example contracts"
  (define blames-this-module?
    (let* ([this-module
            (path->relative-string/library
              (variable-reference->module-source (#%variable-reference)))]
           [blame-rx (regexp (string-append "blaming: " this-module))])
      (λ (x)
        (and (exn:fail:contract:blame? x)
             (regexp-match? blame-rx (exn-message x))))))

  (check-exn blames-this-module?
    (λ () (make-base-eval #:lang #f '(+ 2 2))))
  (check-exn blames-this-module?
    (λ () (make-base-eval #:lang '(+ 2 2))))

  (check-exn blames-this-module?
    (λ () (make-base-eval-factory 'racket/dict)))
  (check-exn blames-this-module?
    (λ () (make-base-eval-factory '() #:lang #f '(+ 2 2))))
  (check-exn blames-this-module?
    (λ () (make-base-eval-factory '() #:lang '(+ 2 2))))

  (check-exn blames-this-module?
    ;; https://github.com/racket/scribble/issues/117
    (λ () (make-eval-factory 'racket/dict)))
  (check-exn blames-this-module?
    (λ () (make-eval-factory '() #:lang #f '(+ 2 2))))
  (check-exn blames-this-module?
    (λ () (make-eval-factory '() #:lang '(+ 2 2))))

  (check-exn blames-this-module?
    (λ () (scribble-exn->string #f)))
  (check-exn blames-this-module?
    (λ () (scribble-exn->string (λ (a b c) a))))
  (check-not-exn
    (λ () (scribble-exn->string)))
  (check-not-exn
    (λ ()
      (parameterize ((scribble-exn->string (λ (a) "hello")))
        ((scribble-exn->string) "error"))))

  (check-exn blames-this-module?
    (λ () (scribble-eval-handler #f)))
  (check-exn blames-this-module?
    (λ () (scribble-eval-handler (λ (ev t) t))))
  (check-not-exn
    (λ () (scribble-eval-handler)))
  (check-not-exn
    (λ ()
      (parameterize ((scribble-eval-handler (λ (a b c) c)))
        ((scribble-eval-handler) (λ (x) x) #f #true))))

  (check-exn blames-this-module?
    (λ () (make-log-based-eval #f 'record)))
  (check-exn blames-this-module?
    (λ () (make-log-based-eval "foo.rkt" 'bad-mode)))

)

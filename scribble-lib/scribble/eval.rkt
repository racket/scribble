#lang racket/base

(require "manual.rkt" "struct.rkt" "scheme.rkt" "decode.rkt"
         (only-in "core.rkt" content? compound-paragraph plain)
         racket/contract/base
         racket/file
         racket/list
         file/convertible ;; attached into new namespace via anchor
         racket/serialize ;; attached into new namespace via anchor
         racket/pretty ;; attached into new namespace via anchor
         scribble/private/serialize ;; attached into new namespace via anchor
         racket/sandbox racket/promise racket/port
         racket/gui/dynamic
         (for-syntax racket/base syntax/srcloc racket/struct)
         racket/stxparam
         racket/splicing
         racket/string
         scribble/text/wrap)

(provide interaction
         interaction0
         interaction/no-prompt
         interaction-eval
         interaction-eval-show
         racketblock+eval (rename-out [racketblock+eval schemeblock+eval])
         racketblock0+eval
         racketmod+eval (rename-out [racketmod+eval schememod+eval])
         def+int
         defs+int
         examples
         examples*
         defexamples
         defexamples*
         as-examples

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
            (parameter/c (-> any/c string?))]
           [scribble-eval-handler
            (parameter/c (-> (-> any/c any) boolean? any/c any))]
           [make-log-based-eval
            (-> path-string? (or/c 'record 'replay) any)])

         with-eval-preserve-source-locations)

(define lang-option/c
  (or/c module-path? (list/c 'special symbol?) (cons/c 'begin list?)))

(define eval-factory/c
  (->* [(listof module-path?)] [#:pretty-print? any/c #:lang lang-option/c] any))

(define scribble-eval-handler
  (make-parameter (lambda (ev c? x) (ev x))))

(define image-counter 0)

(define maxlen 60)

(define-namespace-anchor anchor)

(define (literal-string style s)
  (define m (regexp-match #rx"^(.*)(  +|^ )(.*)$" s))
  (if m
      (make-element #f
                    (list (literal-string style (cadr m))
                          (hspace (string-length (caddr m)))
                          (literal-string style (cadddr m))))
      (make-element style (list s))))

(define list.flow.list (compose1 list make-flow list))

(define (format-output str style)
  (if (string=? "" str)
    '()
    (list (list.flow.list
           (let ([s (regexp-split #rx"\n" (regexp-replace #rx"\n$" str ""))])
             (if (= 1 (length s))
               (make-paragraph (list (literal-string style (car s))))
               (make-table
                #f
                (for/list ([s (in-list s)])
                  (list.flow.list (make-paragraph (list (literal-string style s))))))))))))

(define (format-output-stream in style)
  (define (add-string string-accum line-accum)
    (if string-accum
      (cons (list->string (reverse string-accum))
            (or line-accum null))
      line-accum))
  (define (add-line line-accum flow-accum)
    (if line-accum
      (cons (make-paragraph
             (map (lambda (s)
                    (if (string? s) (literal-string style s) s))
                  (reverse line-accum)))
            flow-accum)
      flow-accum))
  (let loop ([string-accum #f] [line-accum #f] [flow-accum null])
    (define v (read-char-or-special in))
    (cond
      [(eof-object? v)
       (let* ([line-accum (add-string string-accum line-accum)]
              [flow-accum (add-line line-accum flow-accum)])
         (if (null? flow-accum)
             null
             (list (list.flow.list (if (= 1 (length flow-accum))
                                       (car flow-accum)
                                       (make-table #f (map list.flow.list (reverse flow-accum))))))))]
      [(equal? #\newline v) (loop #f #f (add-line (add-string string-accum line-accum) flow-accum))]
      [(char? v) (loop (cons v (or string-accum null)) line-accum flow-accum)]
      [else (loop #f (cons v (or (add-string string-accum line-accum) null)) flow-accum)])))

(define (string->wrapped-lines str)
  (apply
   append
   (for/list ([line-str (regexp-split #rx"\n" str)])
     (wrap-line line-str maxlen
                (λ (word fits)
                   (if ((string-length word) . > . maxlen)
                       (values (substring word 0 fits) (substring word fits) #f)
                       (values #f word #f)))))))

(struct formatted-result (content))

(define (interleave inset? title expr-paras promptless?+val-list+outputs)
  (let ([lines
         (let loop ([expr-paras expr-paras]
                    [promptless?+val-list+outputs promptless?+val-list+outputs]
                    [first? #t]
                    [after-blank? #t])
           (if (null? expr-paras)
             null
             (append
              (if (and (caar promptless?+val-list+outputs)
                       (not after-blank?))
                  (list (list (list blank-line)))
                  null)
              (list (list (let ([p (car expr-paras)])
                            (if (flow? p) p (make-flow (list p))))))
              (format-output (cadr (cdar promptless?+val-list+outputs)) output-color)
              (format-output (caddr (cdar promptless?+val-list+outputs)) error-color)
              (cond
                [(string? (cadar promptless?+val-list+outputs))
                 ;; Error result case:
                 (for/list ([s (in-list (string->wrapped-lines
                                         (cadar promptless?+val-list+outputs)))])
                   (define p (format-output s error-color))
                   (if (null? p)
                       (list null)
                       (car p)))]
                [(box? (cadar promptless?+val-list+outputs))
                 ;; Output written to a port
                 (format-output-stream (unbox (cadar promptless?+val-list+outputs))
                                       result-color)]
                [else
                 ;; Normal result case:
                 (define val-list (cadar promptless?+val-list+outputs))
                 (if (equal? val-list (list (void)))
                     null
                     (map (lambda (v)
                            (list.flow.list
                             (make-paragraph (list (if (formatted-result? v)
                                                       (formatted-result-content v)
                                                       (elem #:style result-color
                                                             (to-element/no-color
                                                              v
                                                              #:expr? (print-as-expression))))))))
                          val-list))])
              (if (and (caar promptless?+val-list+outputs)
                       (pair? (cdr promptless?+val-list+outputs)))
                  (list (list (list blank-line)))
                  null)
              (loop (cdr expr-paras) (cdr promptless?+val-list+outputs) #f (caar promptless?+val-list+outputs)))))])
    (cond
      [inset?
       (let ([p (code-inset (make-table block-color lines))])
         (if title
             (compound-paragraph plain (list title p))
             p))]
      [title (compound-paragraph plain (list title (make-table block-color lines)))]
      [else (make-table block-color lines)])))

;; extracts from a datum or syntax object --- while keeping the
;; syntax-objectness of the original intact, instead of always
;; generating a syntax object or always generating a datum
(define (extract s . ops)
  (let loop ([s s] [ops ops])
    (cond [(null? ops) s]
          [(syntax? s) (loop (syntax-e s) ops)]
          [else (loop ((car ops) s) (cdr ops))])))

(struct nothing-to-eval ())

(struct eval-results (contents out err))
(define (make-eval-results contents out err)
  (unless (and (list? contents)
               (andmap content? contents))
    (raise-argument-error 'eval:results "(listof content?)" contents))
  (unless (string? out)
    (raise-argument-error 'eval:results "string?" out))
  (unless (string? err)
    (raise-argument-error 'eval:results "string?" err))
  (eval-results contents out err))
(define (make-eval-result content out err)
  (unless (content? content)
    (raise-argument-error 'eval:result "content?" content))
  (unless (string? out)
    (raise-argument-error 'eval:result "string?" out))
  (unless (string? err)
    (raise-argument-error 'eval:result "string?" err))
  (eval-results (list content) out err))

(define (extract-to-evaluate s val handle-one)
  (let loop ([val val] [s s] [expect #f] [error-expected? #f] [promptless? #f])
    (syntax-case s (code:line code:comment code:contract eval:no-prompt eval:alts eval:check eval:error)
      [(code:line v (code:comment . rest))
       (loop val (extract s cdr car) expect error-expected? promptless?)]
      [(code:line v ...)
       (for/fold ([val val]) ([v (in-list (extract s cdr))])
         (loop val v expect error-expected? promptless?))]
      [(code:comment . rest)
       (handle-one val (nothing-to-eval) expect error-expected? promptless?)]
      [(code:contract . rest)
       (handle-one val (nothing-to-eval) expect error-expected? promptless?)]
      [(eval:no-prompt e ...)
       (for/fold ([val val]) ([v (in-list (extract s cdr))])
         (handle-one val v expect error-expected? #t))]
      [(eval:error e)
       (handle-one val (extract s cdr car) expect #t promptless?)]
      [(eval:alts p e)
       (handle-one val (extract s cdr cdr car) expect error-expected? promptless?)]
      [(eval:check e expect)
       (handle-one val
                   (extract s cdr car)
                   (list (syntax->datum (datum->syntax #f (extract s cdr cdr car))))
                   error-expected?
                   promptless?)]
      [else (handle-one val s expect error-expected? promptless?)])))

(define (do-eval ev who no-errors?)
  (define (get-outputs)
    (define (get getter what)
      (define s (getter ev))
      (unless (string? s)
        (error who
               "missing ~a, possibly from a sandbox without a `sandbox-~a' configured to 'string"
               what
               (string-join (string-split what) "-")))
      s)
    (list (get get-output "output") (get get-error-output "error output")))
  (define (render-value v)
    (define-values (eval-print eval-print-as-expr?)
      (call-in-sandbox-context ev (lambda () (values (current-print) (print-as-expression)))))
    (cond
      [(and (eq? eval-print (current-print)) eval-print-as-expr?)
       ;; default printer => get result as S-expression
       (make-reader-graph (copy-value v (make-hasheq)))]
      [else
       ;; other printer => go through a pipe
       ;; If it happens to be the pretty printer, tell it to retain
       ;; convertible objects (via write-special)
       (box (call-in-sandbox-context
             ev
             (lambda ()
               (define-values (in out) (make-pipe-with-specials))
               (parameterize ([current-output-port out]
                              [pretty-print-size-hook (lambda (obj _mode _out)
                                                        (and (convertible? obj) 1))]
                              [pretty-print-print-hook
                               (lambda (obj _mode out)
                                 (write-special (if (serializable? obj)
                                                    (make-serialized-convertible (serialize obj))
                                                    obj)
                                                out))])
                 (map (current-print) v))
               (close-output-port out)
               in)))]))
  (define (do-ev/expect s expect error-expected?)
    (define-values (val error? render+output)
      (with-handlers ([(lambda (x) (not (exn:break? x)))
                       (lambda (e)
                         (when (and no-errors?
                                    (not error-expected?))
                           (error 'examples
                                  (string-append "exception raised in example\n"
                                                 "  error: ~s")
                                  (if (exn? e)
                                      (exn-message e)
                                      e)))
                         (values e
                                 #t
                                 (cons ((scribble-exn->string) e)
                                       (get-outputs))))])
        (define val (do-plain-eval ev s #t))
        (values val #f (cons (render-value val) (get-outputs)))))
    (when (and error-expected? (not error?))
      (error 'eval "interaction failed to raise an expected exception: ~.s" s))
    (when expect
      (let ([expect (do-plain-eval ev (car expect) #t)])
        (unless (equal? val expect)
          (define result "  result: ")
          (define expected "  expected: ")
          (error 'eval "example result check failed: ~.s\n~a\n~a\n"
                 s
                 (string-append result (to-lines val (string-length result)))
                 (string-append expected (to-lines expect (string-length expected)))))))
    render+output)

  (define (to-lines exps blank-space)
    (define blank (make-string blank-space #\space))
    (apply
     string-append
     (for/list ([exp (in-list exps)]
                [i (in-naturals)])
       (define first-line? (= i 0))
       (if (= i 0)
           (format "~e" exp)
           (format "\n~a~e" blank exp)))))

  (lambda (str)
    (if (eval-results? str)
        (list #f
              (map formatted-result (eval-results-contents str))
              (eval-results-out str)
              (eval-results-err str))
        (extract-to-evaluate
         str
         (list #f (list (void)) "" "")
         (lambda (result s expect error-expected? promptless?)
          (if (nothing-to-eval? s)
              result
              (cons promptless? (do-ev/expect s expect error-expected?))))))))

(module+ test
  (require rackunit)
  (test-case
   "eval:check in interaction"
   (check-not-exn (λ () (interaction (eval:check #t #t))))))

(define scribble-exn->string
  (make-parameter
   (λ (e)
     (if (exn? e)
         (exn-message e)
         (format "uncaught exception: ~s" e)))))

;; Since we evaluate everything in an interaction before we typeset,
;;  copy each value to avoid side-effects.
(define (copy-value v ht)
  (define (install v v2) (hash-set! ht v v2) v2)
  (let loop ([v v])
    (cond
      [(and v (hash-ref ht v #f)) => (lambda (v) v)]
      [(syntax? v) (make-literal-syntax v)]
      [(string? v) (install v (string-copy v))]
      [(bytes? v) (install v (bytes-copy v))]
      [(pair? v)
       (let ([ph (make-placeholder #f)])
         (hash-set! ht v ph)
         (placeholder-set! ph (cons (loop (car v)) (loop (cdr v))))
         ph)]
      [(mpair? v)
       (let ([p (mcons #f #f)])
         (hash-set! ht v p)
         (set-mcar! p (loop (mcar v)))
         (set-mcdr! p (loop (mcdr v)))
         p)]
      [(vector? v)
       (let ([v2 (make-vector (vector-length v))])
         (hash-set! ht v v2)
         (for ([i (in-range (vector-length v2))])
           (vector-set! v2 i (loop (vector-ref v i))))
         v2)]
      [(box? v)
       (let ([v2 (box #f)])
         (hash-set! ht v v2)
         (set-box! v2 (loop (unbox v)))
         v2)]
      [(hash? v)
       (define ph (make-placeholder #f))
       (hash-set! ht v ph)
       (let ([a (hash-map v (lambda (k v) (cons (loop k) (loop v))))])
         (placeholder-set! ph
                           (cond
                             [(hash-eq? v) (make-hasheq-placeholder a)]
                             [(hash-eqv? v) (make-hasheqv-placeholder a)]
                             [else (make-hash-placeholder a)])))
       ph]
      [else v])))

(define (strip-comments stx)
  (cond
    [(syntax? stx)
     (datum->syntax stx (strip-comments (syntax-e stx)) stx stx)]
    [(pair? stx)
     (define a (car stx))
     (define (comment? a)
       (and (pair? a)
            (or (eq? (car a) 'code:comment)
                (eq? (car a) 'code:contract)
                (and (identifier? (car a))
                     (or (eq? (syntax-e (car a)) 'code:comment)
                         (eq? (syntax-e (car a)) 'code:contract))))))
     (if (or (comment? a) (and (syntax? a) (comment? (syntax-e a))))
       (strip-comments (cdr stx))
       (cons (strip-comments a)
             (strip-comments (cdr stx))))]
    [(eq? stx 'code:blank) (void)]
    [else stx]))

(define (make-base-eval #:lang [lang '(begin)] #:pretty-print? [pretty-print? #t] . ips)
  (call-with-trusted-sandbox-configuration
   (lambda ()
     (parameterize ([sandbox-output 'string]
                    [sandbox-error-output 'string]
                    [sandbox-propagate-breaks #f]
                    [sandbox-namespace-specs
                     (append (sandbox-namespace-specs)
                             (if pretty-print?
                                 '(racket/pretty)
                                 '())
                             '(file/convertible
                               racket/serialize
                               scribble/private/serialize))])
       (define e (apply make-evaluator lang ips))
       (when pretty-print?
         (call-in-sandbox-context
          e
          (lambda () (current-print (dynamic-require 'racket/pretty 'pretty-print-handler)))))
       e))))

(define (make-base-eval-factory mod-paths
                                #:lang [lang '(begin)]
                                #:pretty-print? [pretty-print? #t] . ips)
  (parameterize ([sandbox-namespace-specs
                  (cons (λ () (define ns
                                ;; This namespace-creation choice needs to be consistent
                                ;; with the sandbox (i.e., with `make-base-eval')
                                (if gui?
                                    ((gui-dynamic-require 'make-gui-empty-namespace))
                                    (make-base-empty-namespace)))
                              (parameterize ([current-namespace ns])
                                (for ([mod-path (in-list mod-paths)])
                                  (dynamic-require mod-path #f))
                                (when pretty-print?
                                  (dynamic-require 'racket/pretty #f)))
                              ns)
                        (append mod-paths (if pretty-print? '(racket/pretty) '())))])
    (lambda ()
      (define ev (apply make-base-eval #:lang lang #:pretty-print? #f ips))
      (when pretty-print?
        (call-in-sandbox-context
         ev
         (lambda () (current-print (dynamic-require 'racket/pretty 'pretty-print-handler)))))
      ev)))

(define (make-eval-factory mod-paths
                           #:lang [lang '(begin)]
                           #:pretty-print? [pretty-print? #t] . ips)
  (define base-factory
    (apply make-base-eval-factory mod-paths #:lang lang #:pretty-print? pretty-print? ips))
  (lambda ()
    (let ([ev (base-factory)])
      (call-in-sandbox-context ev
                               (lambda ()
                                 (for ([mod-path (in-list mod-paths)])
                                   (namespace-require mod-path))))
      ev)))

(define (make-log-based-eval logfile mode)
  (case mode
    ((record) (make-eval/record logfile))
    ((replay) (make-eval/replay logfile))))

(define (make-eval/record logfile)
  (define ev (make-base-eval))
  (define super-cust (current-custodian))
  (define out
    (parameterize ([current-custodian (get-user-custodian ev)])
      (open-output-file logfile #:exists 'replace)))
  (display ";; This file was created by make-log-based-eval\n" out)
  (flush-output out)
  (call-in-sandbox-context
   ev
   (lambda ()
     ;; Required for serialization to work.
     (namespace-attach-module (namespace-anchor->namespace anchor) 'racket/serialize)
     (let ([old-eval (current-eval)]
           [init-out-p (current-output-port)]
           [init-err-p (current-error-port)]
           [out-p (open-output-bytes)]
           [err-p (open-output-bytes)])
       (current-eval
        (lambda (x)
          (let* ([x (syntax->datum (datum->syntax #f x))]
                 [x (if (and (pair? x) (eq? (car x) '#%top-interaction))
                        (cdr x)
                        x)]
                 [result (with-handlers ([exn? values])
                           (call-with-values (lambda ()
                                               (parameterize ([current-eval old-eval]
                                                              [current-custodian (make-custodian)]
                                                              [current-output-port out-p]
                                                              [current-error-port err-p])
                                                 (begin0 (old-eval x)
                                                   (wait-for-threads (current-custodian)
                                                                     super-cust))))
                                             list))]
                 [out-s (get-output-bytes out-p #t)]
                 [err-s (get-output-bytes err-p #t)])
            (let ([result* (serialize (cond
                                        [(list? result) (cons 'values result)]
                                        [(exn? result) (list 'exn (exn-message result))]))])
              (pretty-write (list x result* out-s err-s) out)
              (flush-output out))
            (display out-s init-out-p)
            (display err-s init-err-p)
            (cond
              [(list? result) (apply values result)]
              [(exn? result) (raise result)])))))))
  ev)

;; Wait for threads created by evaluation so that the evaluator catches output
;; they generate, etc.
;; FIXME: see what built-in scribble evaluators do
(define (wait-for-threads sub-cust super-cust)
  (define give-up-evt (alarm-evt (+ (current-inexact-milliseconds) 200.0)))
  ;; find a thread to wait on
  (define (find-thread cust)
    (let* ([managed (custodian-managed-list cust super-cust)]
           [thds (filter thread? managed)]
           [custs (filter custodian? managed)])
      (cond
        [(pair? thds) (car thds)]
        [else (ormap find-thread custs)])))
  ;; keep waiting on threads (one at a time) until time to give up
  (define (wait-loop cust)
    (let ([thd (find-thread cust)])
      (when thd
        (cond
          [(eq? give-up-evt (sync thd give-up-evt)) (void)]
          [else (wait-loop cust)]))))
  (wait-loop sub-cust))

(define (make-eval/replay logfile)
  (define ev (make-base-eval))
  (define evaluations (file->list logfile))
  (call-in-sandbox-context
   ev
   (lambda ()
     (namespace-attach-module (namespace-anchor->namespace anchor) 'racket/serialize)
     (let ([old-eval (current-eval)]
           [init-out-p (current-output-port)]
           [init-err-p (current-error-port)])
       (current-eval
        (lambda (x)
          (let* ([x (syntax->datum (datum->syntax #f x))]
                 [x (if (and (pair? x) (eq? (car x) '#%top-interaction))
                        (cdr x)
                        x)])
            (unless (and (pair? evaluations) (equal? x (car (car evaluations))))
              ;; TODO: smarter resync
              ;;  - can handle *additions* by removing next set!
              ;;  - can handle *deletions* by searching forward (but may jump to far
              ;;    if terms occur more than once, eg for stateful code)
              ;; For now, just fail early and often.
              (set! evaluations null)
              (error 'eval "unable to replay evaluation of ~.s" x))
            (let* ([evaluation (car evaluations)]
                   [result (parameterize ([current-eval old-eval])
                             (deserialize (cadr evaluation)))]
                   [result (case (car result)
                             [(values) (cdr result)]
                             [(exn) (make-exn (cadr result) (current-continuation-marks))])]
                   [output (caddr evaluation)]
                   [error-output (cadddr evaluation)])
              (set! evaluations (cdr evaluations))
              (display output init-out-p #| (current-output-port) |#)
              (display error-output init-err-p #| (current-error-port) |#)
              (cond
                [(exn? result) (raise result)]
                [(list? result) (apply values result)]))))))))
  ev)

(define (close-eval e)
  (kill-evaluator e)
  "")

(define (do-plain-eval ev s catching-exns?)
  (parameterize ([sandbox-propagate-breaks #f])
    (call-with-values
        (lambda ()
          ((scribble-eval-handler)
           ev
           catching-exns?
           (let ([s (strip-comments s)])
             (cond [(syntax? s)
                    (syntax-case s (module)
                      [(module . _rest) (syntax->datum s)]
                      [_else s])]
                   ;; a sandbox treats strings and byte strings as code
                   ;; streams, so protect them as syntax objects:
                   [(string? s) (datum->syntax #f s)]
                   [(bytes? s) (datum->syntax #f s)]
                   [else s]))))
        list)))

(define-syntax-parameter quote-expr-preserve-source? #f)

(define-syntax (with-eval-preserve-source-locations stx)
  (syntax-case stx ()
    [(with-eval-preserve-source-locations e ...)
     (syntax/loc stx
       (splicing-syntax-parameterize ([quote-expr-preserve-source? #t])
         e ...))]))

;; Quote an expression to be evaluated or wrap as escaped:
(define-syntax quote-expr
  (syntax-rules (eval:alts eval:result eval:results)
    [(_ (eval:alts e1 e2)) (quote-expr e2)]
    [(_ (eval:result e)) (make-eval-result (list e) "" "")]
    [(_ (eval:result e out)) (make-eval-result (list e) out "")]
    [(_ (eval:result e out err)) (make-eval-result (list e) out err)]
    [(_ (eval:results es)) (make-eval-results es "" "")]
    [(_ (eval:results es out)) (make-eval-results es out "")]
    [(_ (eval:results es out err)) (make-eval-results es out err)]
    [(_ e) (base-quote-expr e)]))

(define orig-stx (read-syntax 'orig (open-input-string "()")))

(define-syntax (base-quote-expr stx)
  (syntax-case stx ()
    [(_ e)
     (cond [(syntax-parameter-value #'quote-expr-preserve-source?)
            ;; Preserve source; produce an expression resulting in a
            ;; syntax object with no lexical context (like strip-context)
            ;; but with (quotable) source locations.
            ;; Also preserve syntax-original?, since that seems important
            ;; to some syntax-based code (eg redex term->pict).
            (define (get-source-location e)
              (let* ([src (build-source-location-list e)]
                     [old-source (source-location-source src)]
                     [new-source
                      (cond [(path? old-source) ;; not quotable/writable
                             ;;(path->string old-source) ;; don't leak build paths
                             'eval]
                            [(or (string? old-source)
                                 (symbol? old-source))
                             ;; Okay? Or should this be replaced also?
                             old-source]
                            [else #f])])
                (update-source-location src #:source new-source)))
            (let loop ([e #'e])
              (cond [(syntax? e)
                     (let ([src (get-source-location e)]
                           [original? (syntax-original? (syntax-local-introduce e))])
                       #`(syntax-property
                          (datum->syntax #f
                                         #,(loop (syntax-e e))
                                         (quote #,src)
                                         #,(if original? #'orig-stx #'#f))
                          'paren-shape
                          (quote #,(syntax-property e 'paren-shape))))]
                    [(pair? e)
                     #`(cons #,(loop (car e)) #,(loop (cdr e)))]
                    [(vector? e)
                     #`(list->vector #,(loop (vector->list e)))]
                    [(box? e)
                     #`(box #,(loop (unbox e)))]
                    [(prefab-struct-key e)
                     => (lambda (key)
                          #`(apply make-prefab-struct
                                   (quote #,key)
                                   #,(loop (struct->list e))))]
                    [else
                     #`(quote #,e)]))]
           [else
            ;; Using quote means that sandbox evaluation works on
            ;; sexprs; to get it to work on syntaxes, use
            ;;   (strip-context (quote-syntax e)))
            ;; while importing
            ;;   (require syntax/strip-context)
            #'(quote e)])]))

(define (do-interaction-eval ev es)
  (for/fold ([ev ev]) ([e (in-list es)])
    (extract-to-evaluate
     e
     ev
     (lambda (ev e expect error-expected?/ignored promptless?/ignored)
       (cond
        [(nothing-to-eval? e) ev]
        [else
         (parameterize ([current-command-line-arguments #()])
           (let ([ev (or ev (make-base-eval))])
             (do-plain-eval ev e #f)
             ev))]))))
  "")

(define-syntax interaction-eval
  (syntax-rules ()
    [(_ #:eval ev e ...) (do-interaction-eval ev (list (quote-expr e) ...))]
    [(_ e ...) (do-interaction-eval #f (list (quote-expr e) ...))]))

(define (show-val v)
  (elem #:style result-color
        (to-element/no-color v #:expr? (print-as-expression))))

(define (do-interaction-eval-show ev es)
  (parameterize ([current-command-line-arguments #()])
    (let ([ev (or ev (make-base-eval))])
      (show-val (car (for/fold ([v (list #f)]) ([e (in-list es)])
                       (extract-to-evaluate
                        e
                        v
                        (lambda (prev-v e expect error-expected?/ignored promptless?/ignored)
                          (do-plain-eval ev e #f)))))))))

(define-syntax interaction-eval-show
  (syntax-rules ()
    [(_ #:eval ev e ...) (do-interaction-eval-show ev (list (quote-expr e) ...))]
    [(_ e ...) (do-interaction-eval-show #f (list (quote-expr e) ...))]))

(define-syntax racketinput*
  (syntax-rules (eval:alts code:comment eval:check eval:no-prompt eval:error eval:result eval:results)
    [(_ #:escape id (code:comment . rest)) (racketblock0 #:escape id (code:comment . rest))]
    [(_ #:escape id (eval:alts a b)) (racketinput* #:escape id a)]
    [(_ #:escape id (eval:result a . _)) (racketinput* #:escape id a)]
    [(_ #:escape id (eval:results a . _)) (racketinput* #:escape id a)]
    [(_ #:escape id (eval:check a b)) (racketinput* #:escape id a)]
    [(_ #:escape id (eval:error a)) (racketinput* #:escape id a)]
    [(_ #:escape id (eval:no-prompt a ...)) (racketblock* #:escape id (code:line a ...))]
    [(_ #:escape id e) (racketinput0 #:escape id e)]))

(define-syntax racketblock*
  (syntax-rules (eval:alts code:comment eval:check eval:no-prompt eval:error eval:result eval:results)
    [(_ #:escape id (code:comment . rest)) (racketblock0 #:escape id (code:comment . rest))]
    [(_ #:escape id (eval:alts a b)) (racketblock* #:escape id a)]
    [(_ #:escape id (eval:result a . _)) (racketinputblock #:escape id a)]
    [(_ #:escape id (eval:results a . _)) (racketinputblock #:escape id a)]
    [(_ #:escape id (eval:check a b)) (racketblock #:escape id a)]
    [(_ #:escape id (eval:no-prompt a ...)) (racketblock #:escape id (code:line a ...))]
    [(_ #:escape id (eval:error a)) (racketblock #:escape id a)]
    [(_ #:escape id e) (racketblock0 #:escape id e)]))

(define-code racketblock0+line (to-paragraph/prefix "" "" (list " ")))

(define-syntax (racketdefinput* stx)
  (syntax-case stx (define define-values define-struct)
    [(_ #:escape id (define . rest))
     (syntax-case stx ()
       [(_ #:escape _ e) #'(racketblock0+line #:escape id e)])]
    [(_ #:escape id (define-values . rest))
     (syntax-case stx ()
       [(_ #:escape _ e) #'(racketblock0+line #:escape id e)])]
    [(_ #:escape id (define-struct . rest))
     (syntax-case stx ()
       [(_ #:escape _ e) #'(racketblock0+line #:escape id e)])]
    [(_ #:escape id (code:line (define . rest) . rest2))
     (syntax-case stx ()
       [(_ #:escape _ e) #'(racketblock0+line #:escape id e)])]
    [(_ #:escape id e) #'(racketinput* #:escape id e)]))

(define (do-titled-interaction who inset? no-errors? ev t shows evals)
  (interleave inset? t shows (map (do-eval ev who no-errors?) evals)))

(define-syntax titled-interaction
  (syntax-rules ()
    [(_ who inset? t racketinput* 
        #:eval ev #:escape unsyntax-id #:no-errors? no-errors?
        e ...)
     (do-titled-interaction
      'who inset? no-errors? ev t
      (list (racketinput* #:escape unsyntax-id e) ...)
      (list (quote-expr e) ...))]
    
    [(_ who inset? t racketinput*
        #:eval ev #:escape unsyntax-id
        e ...)
     (titled-interaction
      who inset? t racketinput*
      #:eval ev #:escape unsyntax-id #:no-errors? #f
      e ...)]
    [(_ who inset? t racketinput*
        #:eval ev #:no-errors? no-errors?
        e ...)
     (titled-interaction
      who inset? t racketinput*
      #:eval ev #:escape unsyntax #:no-errors? no-errors?
      e ...)]
    [(_ who inset? t racketinput*
        #:escape unsyntax-id #:no-errors? no-errors?
        e ...)
     (titled-interaction
      who inset? t racketinput*
      #:eval (make-base-eval) #:escape unsyntax-id #:no-errors? no-errors?
      e ...)]
    [(_ who inset? t racketinput*
        #:eval ev
        e ...)
     (titled-interaction
      who inset? t racketinput*
      #:eval ev #:escape unsyntax #:no-errors? #f
      e ...)]
    [(_ who inset? t racketinput*
        #:escape unsyntax-id
        e ...)
     (titled-interaction
      who inset? t racketinput*
      #:eval (make-base-eval) #:escape unsyntax-id
      e ...)]    
    [(_ who inset? t racketinput*
        #:no-errors? no-errors?
        e ...)
     (titled-interaction
      who inset? t racketinput*
      #:eval (make-base-eval) #:escape unsyntax #:no-errors? no-errors?
      e ...)]
    [(_ who inset? t racketinput* e ...)
     (titled-interaction
      who inset? t racketinput*
      #:eval (make-base-eval) #:escape unsyntax #:no-errors? #f
      e ...)]))

(define-syntax (-interaction stx)
  (syntax-case stx ()
    [(_ who e ...)
     (syntax/loc stx
       (titled-interaction who #f #f racketinput* e ...))]))

(define-syntax (interaction stx)
  (syntax-case stx ()
    [(H e ...) (syntax/loc stx (code-inset (-interaction H e ...)))]))

(define-syntax (interaction/no-prompt stx)
  (syntax-case stx ()
    [(H e ...)
     (syntax/loc stx
       (code-inset (titled-interaction who #f #f racketblock* e ...)))]))

(define-syntax (interaction0 stx)
  (syntax-case stx ()
    [(H e ...) (syntax/loc stx (-interaction H e ...))]))

(define-syntax racketblockX+eval
  (syntax-rules ()
    [(_ racketblock #:eval ev #:escape unsyntax-id e ...)
     (let ([eva ev])
       (#%expression
        (begin (interaction-eval #:eval eva e ...)
               (racketblock #:escape unsyntax-id e ...))))]
    [(_ racketblock #:eval ev e ...)
     (racketblockX+eval racketblock #:eval ev #:escape unsyntax e ...)]
    [(_ racketblock #:escape unsyntax-id e ...)
     (racketblockX+eval racketblock #:eval (make-base-eval) #:escape unsyntax-id e ...)]
    [(_ racketblock e ...)
     (racketblockX+eval racketblock #:eval (make-base-eval) #:escape unsyntax e ...)]))

(define-syntax racketblock+eval
  (syntax-rules ()
    [(_ e ...)
     (racketblockX+eval racketblock e ...)]))

(define-syntax racketblock0+eval
  (syntax-rules ()
    [(_ e ...)
     (racketblockX+eval racketblock0 e ...)]))

(define-syntax racketmod+eval
  (syntax-rules ()
    [(_ #:eval ev #:escape unsyntax-id name e ...)
     (let ([eva ev])
       (#%expression
        (begin (interaction-eval #:eval eva e ...)
               (racketmod #:escape unsyntax-id name e ...))))]
    [(_ #:eval ev name e ...)
     (racketmod+eval #:eval ev #:escape unsyntax name e ...)]
    [(_ #:escape unsyntax-id name e ...)
     (racketmod+eval #:eval (make-base-eval) #:escape unsyntax-id name e ...)]
    [(_ name e ...)
     (racketmod+eval #:eval (make-base-eval) #:escape unsyntax name e ...)]))

(define-syntax (defs+int stx)
  (syntax-case stx ()
    [(H #:eval ev #:escape unsyntax-id [def ...] e ...)
     (syntax/loc stx
       (let ([eva ev])
         (column (list (racketblock0+eval #:eval eva #:escape unsyntax-id def ...)
                       blank-line
                       (-interaction H #:eval eva #:escape unsyntax-id e ...)))))]
    [(H #:eval ev [def ...] e ...)
     (syntax/loc stx (defs+int #:eval ev #:escape unsyntax [def ...] e ...))]
    [(_ #:escape unsyntax-id [def ...] e ...)
     (syntax/loc stx (defs+int #:eval (make-base-eval) #:escape unsyntax-id [def ...] e ...))]
    [(_ [def ...] e ...)
     (syntax/loc stx (defs+int #:eval (make-base-eval) [def ...] e ...))]))

(define-syntax def+int
  (syntax-rules ()
    [(H #:eval ev #:escape unsyntax-id def e ...)
     (defs+int #:eval ev #:escape unsyntax-id [def] e ...)]
    [(H #:eval ev def e ...)
     (defs+int #:eval ev [def] e ...)]
    [(H #:escape unsyntax-id def e ...)
     (defs+int #:escape unsyntax-id [def] e ...)]
    [(H def e ...)
     (defs+int [def] e ...)]))

(define example-title
  (make-paragraph (list "Example:")))
(define examples-title
  (make-paragraph (list "Examples:")))

(define-syntax pick-example-title
  (syntax-rules ()
    [(_ e) example-title]
    [(_ #:eval ev e) example-title]
    [(_ #:escape id e) example-title]
    [(_ #:eval ev #:escape id e) example-title]
    [(_ . _) examples-title]))

(define-syntax (examples stx)
  (syntax-case stx ()
    [(H e ...)
     (syntax/loc stx
       (titled-interaction
        H #t (pick-example-title e ...)  racketinput* e ...))]))
(define-syntax (examples* stx)
  (syntax-case stx ()
    [(H example-title e ...)
     (syntax/loc stx
       (titled-interaction H #t example-title racketinput* e ...))]))
(define-syntax (defexamples stx)
  (syntax-case stx ()
    [(H e ...)
     (syntax/loc stx
       (titled-interaction
        H #t (pick-example-title e ...)  racketdefinput* e ...))]))
(define-syntax (defexamples* stx)
  (syntax-case stx ()
    [(H example-title e ...)
     (syntax/loc stx
       (titled-interaction H #t example-title racketdefinput* e ...))]))

(define blank-line (make-paragraph (list 'nbsp)))

(define (column l)
  (code-inset (make-table #f (map list.flow.list l))))

(define (do-splice l)
  (cond [(null? l) null]
        [(splice? (car l)) `(,@(splice-run (car l)) ,@(do-splice (cdr l)))]
        [else (cons (car l) (do-splice (cdr l)))]))

(define as-examples
  (case-lambda
    [(t) (as-examples examples-title t)]
    [(example-title t)
     (if example-title
         (compound-paragraph
          plain
          (list
           (if (block? example-title)
               example-title
               (make-paragraph (list example-title)))
           t))
         t)]))

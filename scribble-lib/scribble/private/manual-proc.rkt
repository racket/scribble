#lang racket/base
(require "../struct.rkt"
         "../scheme.rkt"
         "../basic.rkt"
         "../manual-struct.rkt"
         (only-in "../core.rkt" 
                  make-style make-table-columns)
         "../html-properties.rkt"
         "qsloc.rkt"
         "manual-utils.rkt"
         "manual-vars.rkt"
         "manual-style.rkt"
         "manual-scheme.rkt"
         "manual-bind.rkt"
         "manual-method.rkt"
         "manual-ex.rkt"
         "on-demand.rkt"
         scheme/string
         scheme/list
         (for-syntax racket/base
                     syntax/parse)
         (for-label racket/base
                    racket/contract
                    racket/class))

(provide defproc defproc* defstruct defstruct*
         defparam defparam* defboolparam
         defthing defthing* 
         defthing/proc ; XXX unknown contract
         ;; private:
         *defthing) ; XXX unknown contract

(define-on-demand dots0
  (make-element meta-color (list "...")))
(define-on-demand dots1
  (make-element meta-color (list "...+")))

(define (make-openers n)
  (racketparenfont
   (case n [(1) "("] [(0) ""] [(2) "(("] [else (make-string n #\()])))
(define (make-closers n)
  (racketparenfont
   (case n [(1) ")"] [(0) ""] [(2) "))"] [else (make-string n #\))])))

(define-syntax (arg-contract stx)
  (syntax-case stx (... ...+ _...superclass-args...)
    [(_ [id contract])
     (identifier? #'id)
     #'(racketblock0 contract)]
    [(_ [id contract val])
     (identifier? #'id)
     #'(racketblock0 contract)]
    [(_ [kw id contract])
     (and (keyword? (syntax-e #'kw)) (identifier? #'id))
     #'(racketblock0 contract)]
    [(_ [kw id contract val])
     (and (keyword? (syntax-e #'kw)) (identifier? #'id))
     #'(racketblock0 contract)]
    [(_ (... ...)) #'#f]
    [(_ (... ...+)) #'#f]
    [(_ _...superclass-args...) #'#f]
    [(_ arg) (raise-syntax-error 'defproc "bad argument form" #'arg)]))

(define-syntax (arg-default stx)
  (syntax-case stx (... ...+ _...superclass-args...)
    [(_ [id contract])
     (identifier? #'id)
     #'#f]
    [(_ [id contract val])
     (identifier? #'id)
     #'(racketblock0 val)]
    [(_ [kw id contract])
     (keyword? (syntax-e #'kw))
     #'#f]
    [(_ [kw id contract val])
     (keyword? (syntax-e #'kw))
     #'(racketblock0 val)]
    [_ #'#f]))

(define-syntax (extract-proc-id stx)
  (syntax-case stx ()
    [(_ k e id)
     (identifier? #'id)
     (if (and (syntax-e #'k)
              (free-identifier=? #'k #'id))
         #'e
         #`(quote-syntax/loc id))]
    [(_ k e (proto arg ...))
     #'(extract-proc-id k e proto)]
    [(_ thing) (raise-syntax-error 'defproc "bad prototype" #'thing)]))

(define-syntax (arg-contracts stx)
  (syntax-case stx ()
    [(_ id arg ...)
     (identifier? #'id)
     #'(list (lambda () (arg-contract arg)) ...)]
    [(_ (proto arg1 ...) arg ...)
     #'(arg-contracts proto arg1 ... arg ...)]
    [_ (raise-syntax-error 'defproc "bad prototype" stx)]))

(define-syntax (arg-defaults stx)
  (syntax-case stx ()
    [(_ id arg ...)
     (identifier? #'id)
     #'(list (lambda () (arg-default arg)) ...)]
    [(_ (proto arg1 ...) arg ...)
     #'(arg-defaults proto arg1 ... arg ...)]
    [_ (raise-syntax-error 'defproc "bad prototype" stx)]))

(define-syntax (result-contract stx)
  (syntax-case stx (values)
    [(_ (values c ...))
     #'(list (racketblock0 c) ...)]
    [(_ c)
     (if (string? (syntax-e #'c))
       (raise-syntax-error 'defproc
                           "expected a result contract, found a string" #'c)
       #'(racketblock0 c))]))

(define no-value #f)

(define-syntax (result-value stx)
  (syntax-case stx (no-value let)
    [(_ no-value)  #'#f]
    [(_ (let () e ...))  #'(racketblock0 e ...)]
    [(_ v)  #'(racketblock0 v)]))

(begin-for-syntax
 (define-splicing-syntax-class kind-kw
   #:description "#:kind keyword"
   (pattern (~optional (~seq #:kind kind)
                       #:defaults ([kind #'#f]))))

 (define-splicing-syntax-class value-kw
   #:description "#:value keyword"
   (pattern (~optional (~seq #:value value)
                       #:defaults ([value #'no-value]))))
 
 (define-splicing-syntax-class link-target?-kw
   #:description "#:link-target? keyword"
   (pattern (~seq #:link-target? expr))
   (pattern (~seq)
            #:with expr #'#t))

 (define-syntax-class id-or-false
   (pattern i:id)
   (pattern #f #:with i #'#f))
   
 (define-splicing-syntax-class id-kw
   #:description "#:id keyword"
   (pattern (~optional (~seq #:id [key:id-or-false expr])
                       #:defaults ([key #'#f]
                                   [expr #'#f]))))
 
 (define-splicing-syntax-class mode-kw
   #:description "#:mode keyword"
   (pattern (~optional (~seq #:mode m:id)
                       #:defaults ([m #'procedure]))))

 (define-splicing-syntax-class within-kw
   #:description "#:within keyword"
   (pattern (~optional (~seq #:within cl:id)
                       #:defaults ([cl #'#f]))))
 )

(define-syntax (defproc stx)
  (syntax-parse stx
    [(_ kind:kind-kw
        lt:link-target?-kw
        i:id-kw
        (id arg ...)
        result
        value:value-kw
        desc ...)
     (syntax/loc stx
       (defproc*
         #:kind kind.kind
         #:link-target? lt.expr
         #:id [i.key i.expr]
         [[(id arg ...) result #:value value.value]]
         desc ...))]))

(define-syntax (defproc* stx)
  (syntax-parse stx
    [(_ kind:kind-kw
        lt:link-target?-kw
        d:id-kw
        mode:mode-kw
        within:within-kw
        [[proto result value:value-kw] ...]
        desc ...)
     (syntax/loc stx
       (with-togetherable-racket-variables
        ()
        ([proc proto] ...)
        (let ([alt-id d.expr])
          (*defproc kind.kind
                    lt.expr
                    'mode.m (quote-syntax/loc within.cl)
                    (list (extract-proc-id d.key alt-id proto) ...)
                    'd.key
                    '[proto ...]
                    (list (arg-contracts proto) ...)
                    (list (arg-defaults proto) ...)
                    (list (lambda () (result-contract result)) ...)
                    (lambda () (list desc ...))
                    (list (result-value value.value) ...)))))]))

(define-struct arg
  (special? kw id optional? starts-optional? ends-optional? depth))

(define (*defproc kind link? mode within-id
                  stx-ids sym prototypes arg-contractss arg-valss result-contracts content-thunk
                  [result-values (map (lambda (x) #f) result-contracts)])
  (define max-proto-width (current-display-width))
  (define ((arg->elem show-opt-start?) arg next-depth)
    (let* ([e (cond [(not (arg-special? arg))
                     (if (arg-kw arg)
                       (if (eq? mode 'new)
                         (make-element
                          #f (list (racketparenfont "[")
                                   (racketidfont (datum-intern-literal (keyword->string (arg-kw arg))))
                                   spacer
                                   (to-element (make-var-id (arg-id arg)))
                                   (racketparenfont "]")))
                         (make-element
                          #f (list (to-element (arg-kw arg))
                                   spacer
                                   (to-element (make-var-id (arg-id arg))))))
                       (to-element (make-var-id (arg-id arg))))]
                    [(eq? (arg-id arg) '...+) dots1]
                    [(eq? (arg-id arg) '...) dots0]
                    [(eq? (arg-id arg) '_...superclass-args...) (to-element (arg-id arg))]
                    [else (to-element (make-var-id (arg-id arg)))])]
           [e (if (arg-ends-optional? arg)
                (make-element #f (list e "]"))
                e)]
           [num-closers (- (arg-depth arg) next-depth)]
           [e (if (zero? num-closers)
                e
                (make-element
                 #f (list e (make-closers num-closers))))])
      (if (and show-opt-start? (arg-starts-optional? arg))
        (make-element #f (list "[" e))
        e)))
  (define (prototype-depth p)
    (let loop ([p (car p)])
      (if (symbol? p) 0 (+ 1 (loop (car p))))))
  (define (prototype-args p)
    (define (parse-arg v in-optional? depth next-optional? next-special-dots?)
      (let* ([id (if (pair? v) ((if (keyword? (car v)) cadr car) v) v)]
             [kw (and (pair? v) (keyword? (car v)) (car v))]
             [default? (and (pair? v) (pair? ((if kw cdddr cddr) v)))])
        (make-arg (symbol? v) kw id default?
                  (and default? (not in-optional?))
                  (or (and (not default?)
                           in-optional?) ; => must be special
                      (and default?
                           (not next-optional?)
                           (not next-special-dots?)))
                  depth)))
    (let loop ([p p] [depth 0])
      (define head
        (if (symbol? (car p))
            null
            (loop (car p) (add1 depth))))
      (append
       head
       (let loop ([p (cdr p)] [in-optional? #f])
         (cond
           [(null? p) null]
           [(null? (cdr p))
            (list (parse-arg (car p) in-optional? depth #f #f))]
           [else
            (let ([a (parse-arg
                      (car p)
                      in-optional?
                      depth
                      (let ([v (cadr p)])
                        (and (pair? v)
                             (not
                              (null? ((if (keyword? (car v)) cdddr cddr) v)))))
                      (and (not (pair? (cadr p)))
                           (not (eq? '_...superclass-args... (cadr p)))))])
              (cons a (loop (cdr p)
                            (and (arg-optional? a)
                                 (not (arg-ends-optional? a))))))])))))
  (define (next-args-depth args)
    (if (null? args)
        0
        (arg-depth (car args))))
  (define (prototype-size args first-combine next-combine special-combine?)
    (let loop ([s args] [combine first-combine])
      (if (null? s)
        0
        (combine
         (loop (cdr s) next-combine)
         (let ([a (car s)])
           (+ (- (arg-depth a) (next-args-depth (cdr s)))
              (if (arg-special? a)
                (string-length (symbol->string (arg-id a)))
                (+ (if (arg-kw a)
                     (+ (if (eq? mode 'new) 2 0)
                        (string-length (keyword->string (arg-kw a)))
                        3
                        (string-length (symbol->string (arg-id a))))
                     (string-length (symbol->string (arg-id a))))
                   (if (and special-combine?
                            (pair? (cdr s))
                            (arg-special? (cadr s))
                            (not (eq? '_...superclass-args...
                                      (arg-id (cadr s)))))
                     (+ 1 (string-length (symbol->string (arg-id (cadr s)))))
                     0)))))))))
  (define (extract-id p stx-id)
    (let loop ([p p])
      (if (symbol? (car p)) 
          (let ([s (car p)])
            (if (eq? s sym)
                (syntax-e stx-id)
                (car p)))
          (loop (car p)))))
  (define (do-one stx-id prototype args arg-contracts arg-vals result-contract result-value
                  first? add-background-label?)
    (let ([names (remq* '(... ...+) (map arg-id args))])
      (unless (= (length names) (length (remove-duplicates names eq?)))
        (error 'defproc "duplicate argument names in prototype for ~s: ~s"
               (syntax->datum stx-id) names)))
    (define tagged
      (cond
        [(or (eq? mode 'new)
             (eq? mode 'make))
         (define content
           (list (if (eq? mode 'new)
                     (racket new)
                     (racket make-object))))
         (define new-elem
           (if (and first? link?)
               (let* ([target-maker (id-to-target-maker within-id #f)])
                 (if target-maker
                     (target-maker
                      content
                      (lambda (ctag)
                        (let ([tag (constructor-tag ctag)])
                          (make-toc-target-element
                           #f
                           (list (make-index-element
                                  #f
                                  content
                                  tag
                                  (list (datum-intern-literal (symbol->string (syntax-e within-id))) 
                                        (if (eq? mode 'new)
                                            "new"
                                            "make-object"))
                                  content
                                  (with-exporting-libraries
                                   (lambda (libs)
                                     (make-constructor-index-desc
                                      (syntax-e within-id)
                                      libs ctag)))))
                           tag))))
                     (car content)))
               (car content)))
         (make-element #f (list new-elem spacer (to-element within-id)))]
        [(eq? mode 'send)
         (make-element
          #f
          (list (racket send) spacer
                (name-this-object (syntax-e within-id)) spacer
                (if (and first? link?)
                  (let* ([mname (extract-id prototype stx-id)]
                         [target-maker (id-to-target-maker within-id #f)]
                         [content (*method mname within-id #:defn? #t)]
                         [ref-content (*method mname within-id)])
                    (if target-maker
                      (target-maker
                       content
                       (lambda (ctag)
                         (let ([tag (method-tag ctag mname)])
                           (make-toc-target2-element
                            #f
                            (list (make-index-element
                                   #f
                                   content
                                   tag
                                   (list (datum-intern-literal (symbol->string mname)))
                                   (list ref-content)
                                   (with-exporting-libraries
                                    (lambda (libs)
                                      (make-method-index-desc
                                       (syntax-e within-id)
                                       libs mname ctag)))))
                            tag
                            ref-content))))
                      content))
                  (*method (extract-id prototype stx-id) within-id #:defn? #t))))]
        [(and first? link?)
         (define the-id (extract-id prototype stx-id))
         (let ([target-maker (id-to-target-maker stx-id #t)])
           (define-values (content ref-content) (definition-site the-id stx-id #f))
           (if target-maker
               (target-maker
                content
                (lambda (tag)
                  (make-toc-target2-element
                   #f
                   (make-index-element
                    #f content tag
                    (list (datum-intern-literal (symbol->string the-id)))
                    (list ref-content)
                    (with-exporting-libraries
                     (lambda (libs)
                       (make-procedure-index-desc the-id libs))))
                   tag
                   ref-content)))
               content))]
        [else
         (define the-id (extract-id prototype stx-id))
         ((if link? annote-exporting-library values)
          (let ([sig (current-signature)])
            (if sig
              (*sig-elem #:defn? #t (sig-id sig) the-id)
              (to-element #:defn? #t (make-just-context the-id stx-id)))))]))
    (define p-depth (prototype-depth prototype))
    (define flat-size (+ (prototype-size args + + #f)
                         p-depth
                         (element-width tagged)))
    (define short? (or (flat-size . < . 40) ((length args) . < . 2)))
    (define res
      (let ([res (result-contract)])
        (if (list? res)
          ;; multiple results
          (if (null? res)
            'nbsp
            (let ([w (apply + (map block-width res))])
              (if (or (ormap table? res) (w . > . 40))
                (make-table
                 #f (map (lambda (fe) (list (make-flow (list fe)))) res))
                (make-table
                 #f
                 (list (let loop ([res res])
                         (if (null? (cdr res))
                           (list (make-flow (list (car res))))
                           (list* (make-flow (list (car res)))
                                  flow-spacer
                                  (loop (cdr res))))))))))
          res)))
    (define tagged+arg-width (+ (prototype-size args max max #t)
                                p-depth
                                (element-width tagged)))
    (define result-next-line?
      ((+ (if short? flat-size tagged+arg-width) (block-width res))
       . >= . (- max-proto-width 7)))
    (define end (list flow-spacer (to-flow 'rarr)
                      flow-spacer (make-flow (list res))))
    (define (get-label)
      (case mode
        [(new make) "constructor"]
        [(send) "method"]
        [else (or kind "procedure")]))
    (append
     (list
      (list
       ((if add-background-label? (add-background-label (get-label)) values)
        (make-flow
         (if short?
             ;; The single-line case:
             (top-align
              make-table-if-necessary
              "prototype"
              (list
               (cons
                (to-flow
                 (make-element
                  #f
                  `(,(make-openers (add1 p-depth))
                    ,tagged
                    ,(let ([num-closers (- p-depth (next-args-depth args))])
                       (if (zero? num-closers)
                           '()
                           (make-closers num-closers)))
                    ,@(if (null? args)
                          (list (make-closers p-depth))
                          (let loop ([args args])
                            (cond
                              [(null? args) null]
                              [else
                               (append
                                (list spacer ((arg->elem #t) (car args) (next-args-depth (cdr args))))
                                (loop (cdr args)))])))
                    ,(racketparenfont ")"))))
                (if result-next-line? null end))))
             ;; The multi-line case:
             (let ([not-end (if result-next-line?
                                (list flow-spacer)
                                (list flow-spacer flow-spacer
                                      flow-spacer flow-spacer))]
                   [one-ok? (and (not (eq? mode 'new)) (tagged+arg-width . < . (- max-proto-width 5)))])
               (list
                (top-align
                 make-table
                 "prototype"
                 (cons
                  (cons
                   (to-flow
                    (make-element
                     #f
                     (list
                      (make-openers (add1 p-depth))
                      tagged)))
                   (if one-ok?
                       (list*
                        (if (arg-starts-optional? (car args))
                            (to-flow (make-element #f (list spacer "[")))
                            flow-spacer)
                        (to-flow ((arg->elem #f) (car args) (next-args-depth (cdr args))))
                        not-end)
                       (list* 'cont 'cont not-end)))
                  (let loop ([args (if one-ok? (cdr args) args)])
                    (if (null? args)
                        null
                        (let ([dots-next?
                               (or (and (pair? (cdr args))
                                        (arg-special? (cadr args))
                                        (not (eq? '_...superclass-args...
                                                  (arg-id (cadr args))))))])
                          (cons
                           (list*
                            (if (eq? mode 'new)
                                (flow-spacer/n 3)
                                flow-spacer)
                            (if (arg-starts-optional? (car args))
                                (to-flow (make-element #f (list spacer "[")))
                                flow-spacer)
                            (let ([a ((arg->elem #f) (car args) (next-args-depth (cdr args)))]
                                  [next (if dots-next?
                                            (make-element
                                             #f (list spacer
                                                      ((arg->elem #f)
                                                       (cadr args)
                                                       (next-args-depth (cddr args)))))
                                            "")])
                              (to-flow
                               (cond
                                [(null? ((if dots-next? cddr cdr) args))
                                 (make-element
                                  #f
                                  (list a next (racketparenfont ")")))]
                                [(equal? next "") a]
                                [else
                                 (make-element #f (list a next))])))
                            (if (and (null? ((if dots-next? cddr cdr) args))
                                     (not result-next-line?))
                                end
                                not-end))
                           (loop ((if dots-next? cddr cdr)
                                  args)))))))))))))))
     (if result-next-line?
       (list (list (make-flow (top-align
                               make-table-if-necessary
                               "prototype"
                               (list end)))))
       null)
     (append-map
      (lambda (arg arg-contract arg-val)
        (cond
          [(not (arg-special? arg))
           (let* ([arg-cont (arg-contract)]
                  [base-len (+ 5 (string-length (symbol->string (arg-id arg)))
                               (block-width arg-cont))]
                  [arg-val (and arg-val (arg-val))]
                  [def-len (if (arg-optional? arg) (block-width arg-val) 0)]
                  [base-list
                   (list (to-flow (hspace 2))
                         (to-flow (to-element (make-var-id (arg-id arg))))
                         flow-spacer
                         (to-flow ":")
                         flow-spacer
                         (make-flow (list arg-cont)))])
             (list
              (list
               (make-flow
                (if (and (arg-optional? arg)
                         ((+ base-len 3 def-len) . >= . max-proto-width))
                  (list
                   (top-align
                    make-table
                    "argcontract"
                    (list base-list (list flow-spacer flow-spacer flow-spacer
                                          (to-flow "=") flow-spacer
                                          (make-flow (list arg-val))))))
                  (let ([show-default?
                         (and (arg-optional? arg)
                              ((+ base-len 3 def-len) . < . max-proto-width))])
                    (top-align
                     make-table-if-necessary
                     "argcontract"
                     (list
                      (append
                       base-list
                       (if show-default?
                           (list flow-spacer (to-flow "=") flow-spacer
                                 (make-flow (list arg-val)))
                           null))))))))))]
          [else null]))
      args
      arg-contracts
      arg-vals)
     (if result-value
         (let ([result-block  (if (block? result-value)
                                  result-value
                                  (make-omitable-paragraph (list result-value)))])
           (list (list (list (top-align
                              make-table
                              "argcontract"
                              (list (list
                                     (to-flow (make-element #f (list spacer "=" spacer)))
                                     (make-flow (list result-block)))))))))
         null)))
  (define all-args (map prototype-args prototypes))
  (define var-list
    (filter-map (lambda (a) (and (not (arg-special? a)) (arg-id a)))
                (append* all-args)))
  (make-box-splice
   (cons
    (make-blockquote
     vertical-inset-style
     (list
      (make-table
       boxed-style
       (append-map
        do-one
        stx-ids prototypes all-args arg-contractss arg-valss result-contracts result-values
        (let loop ([ps prototypes] [stx-ids stx-ids] [accum null])
          (cond [(null? ps) null]
                [(ormap (lambda (a) (eq? (extract-id (car ps) (car stx-ids)) a)) accum)
                 (cons #f (loop (cdr ps) (cdr stx-ids) accum))]
                [else (cons #t (loop (cdr ps)
                                     (cdr stx-ids)
                                     (cons (extract-id (car ps) (car stx-ids)) accum)))]))
        (for/list ([p (in-list prototypes)]
                   [i (in-naturals)])
          (= i 0))))))
    (content-thunk))))

(define-syntax (defparam stx)
  (syntax-parse stx
    [(_ lt:link-target?-kw id arg contract value:value-kw desc ...)
     #'(defproc* #:kind "parameter" #:link-target? lt.expr
         ([(id) contract] [(id [arg contract]) void? #:value value.value]) 
         desc ...)]))
(define-syntax (defparam* stx)
  (syntax-parse stx
    [(_ lt:link-target?-kw id arg in-contract out-contract value:value-kw desc ...)
     #'(defproc* #:kind "parameter" #:link-target? lt.expr
         ([(id) out-contract] [(id [arg in-contract]) void? #:value value.value])
         desc ...)]))
(define-syntax (defboolparam stx)
  (syntax-parse stx
    [(_ lt:link-target?-kw id arg value:value-kw desc ...)
     #'(defproc* #:kind "parameter" #:link-target? lt.expr
         ([(id) boolean?] [(id [arg any/c]) void? #:value value.value])
         desc ...)]))

(define top-align-styles (make-hash))
(define (top-align make-table style-name cols)
  (if (null? cols)
      (make-table style-name null)
      (let* ([n (length (car cols))]
             [k (cons style-name n)])
        (make-table
         (hash-ref top-align-styles
                   k
                   (lambda ()
                     (define s
                       (make-style style-name
                                   (list (make-table-columns (for/list ([i n])
                                                               (make-style #f '(top)))))))
                     (hash-set! top-align-styles k s)
                     s))
         cols))))

;; ----------------------------------------

(begin-for-syntax
  (define-splicing-syntax-class mutable-kw
    #:description "#:mutable keyword"
    (pattern (~seq #:mutable)
             #:with immutable? #'#f)
    (pattern (~seq)
             #:with immutable? #'#t))
  
  (define-splicing-syntax-class opacity-kw
    #:description "#:prefab, #:transparent, or #:inspector keyword"
    (pattern (~seq #:prefab)
             #:with opacity #''prefab)
    (pattern (~seq #:transparent)
             #:with opacity #''transparent)
    (pattern (~seq #:inspector #f)
             #:with opacity #''transparent)
    (pattern (~seq)
             #:with opacity #''opaque))
  
  (define-splicing-syntax-class constructor-kw
    #:description "#:constructor-name, #:extra-constructor-name, or #:omit-constructor keyword"
    (pattern (~seq #:constructor-name id)
             #:with omit? #'#f
             #:with given? #'#t
             #:with extra? #'#f)
    (pattern (~seq #:extra-constructor-name id)
             #:with omit? #'#f
             #:with given? #'#t
             #:with extra? #'#t)
    (pattern (~seq #:omit-constructor)
             #:with omit? #'#t
             #:with id #'#f
             #:with given? #'#f
             #:with extra? #'#f)
    (pattern (~seq)
             #:with omit? #'#f
             #:with id #'#f
             #:with given? #'#f
             #:with extra? #'#f)))

(define-syntax-rule (define-defstruct defstruct default-extra?)
  (...
   (define-syntax (defstruct stx)
     (syntax-parse stx
       [(_ lt:link-target?-kw name fields 
           m:mutable-kw o:opacity-kw c:constructor-kw 
           desc ...)
        #`(**defstruct lt.expr name fields 
                       m.immutable? o.opacity
                       c.id c.given? c.extra? default-extra? c.omit?
                       desc ...)]))))

(define-defstruct defstruct #t)
(define-defstruct defstruct* #f)

(define-syntax-rule (**defstruct link? name ([field field-contract] ...) 
                                 immutable? opacity 
                                 cname cname-given? extra-cname? default-extra? omit-constructor?
                                 desc ...)
  (with-togetherable-racket-variables
   ()
   ()
   (*defstruct link? (quote-syntax/loc name) 'name 
               (quote-syntax/loc cname) cname-given? extra-cname? default-extra? omit-constructor?
               '([field field-contract] ...)
               (list (lambda () (racketblock0 field-contract)) ...)
               immutable? opacity
               (lambda () (list desc ...)))))

(define (*defstruct link? stx-id name 
                    alt-cname-id cname-given? extra-cname? default-extra? omit-constructor?
                    fields field-contracts 
                    immutable? opacity
                    content-thunk)
  (define transparent? (or (eq? opacity 'transparent)
                           (eq? opacity 'prefab)))
  (define prefab? (eq? opacity 'prefab))
  (define max-proto-width (current-display-width))
  (define (field-name f) ((if (pair? (car f)) caar car) f))
  (define (field-view f)
    (if (pair? (car f)) (make-shaped-parens (car f) #\[) (car f)))
  (define cname-id
    (cond
     [omit-constructor? #f]
     [(identifier? alt-cname-id) alt-cname-id]
     [(not default-extra?) #f]
     [else (let ([name-id (if (identifier? stx-id)
                              stx-id
                              (car (syntax-e stx-id)))])
             (datum->syntax name-id
                            (string->symbol (format "make-~a" (syntax-e name-id)))
                            name-id
                            name-id))]))
  (define keyword-modifiers? (or (not immutable?)
                                 transparent?
                                 cname-id))
  (define keyword-spacer (hspace 4)) ; 2 would match DrRacket indentation, but 4 looks better with field contracts after
  (define main-table
    (make-table
     boxed-style
     (append
      ;; First line in "boxed" table is struct name and fields:
      (list
       (list
        ((add-background-label "struct")
         (list
          (let* ([the-name
                  (let ([just-name
                         (let ([name-id (if (pair? name)
                                            (make-just-context (car name)
                                                               (car (syntax-e stx-id)))
                                            stx-id)])
                           (if link?
                               (let ()
                                 (define (gen defn?)
                                   ((if defn? annote-exporting-library values)
                                    (to-element #:defn? defn? name-id)))
                                 (define content (gen #t))
                                 (define ref-content (gen #f))
                                 (make-target-element*
                                  (lambda (s c t)
                                    (make-toc-target2-element s c t ref-content))
                                  (if (pair? name)
                                      (car (syntax-e stx-id))
                                      stx-id)
                                  content
                                  (let ([name (if (pair? name) (car name) name)])
                                    (list* (list 'info name)
                                           (list 'type 'struct: name)
                                           (list 'predicate name '?)
                                           (append
                                            (if cname-id
                                                (list (list 'constructor (syntax-e cname-id)))
                                                null)
                                            (map (lambda (f)
                                                   (list 'accessor name '-
                                                         (field-name f)))
                                                 fields)
                                            (filter-map
                                             (lambda (f)
                                               (if (or (not immutable?)
                                                       (and (pair? (car f))
                                                            (memq '#:mutable
                                                                  (car f))))
                                                   (list 'mutator 'set- name '-
                                                         (field-name f) '!)
                                                   #f))
                                             fields))))))
                               (to-element #:defn? #t name-id)))])
                    (if (pair? name)
                        (make-element
                         #f
                         (list just-name
                               (hspace 1)
                               (to-element
                                (make-just-context
                                 (cadr name)
                                 (cadr (syntax-e stx-id))))))
                        just-name))]
                 [sym-length (lambda (s)
                               (string-length (symbol->string s)))]
                 [short-width
                  (apply +
                         (length fields) ; spaces between field names
                         8 ; "struct" + "(" + ")"
                         (append
                          (map sym-length
                               (append (if (pair? name) name (list name))
                                       (map field-name fields)))
                          (map (lambda (f)
                                 (if (pair? (car f))
                                     (+ 3 2 (string-length (keyword->string
                                                            (cadar f))))
                                     0))
                               fields)))])
            (if (and (short-width . < . max-proto-width)
                     (not keyword-modifiers?))
                ;; All on one line:
                (make-omitable-paragraph
                 (list
                  (to-element
                   `(,(racket struct)
                     ,the-name
                     ,(map field-view fields)))))
                ;; Multi-line view (leaving out last paren if keywords follow):
                (let* ([one-right-column?
                        ;; Does the struct name and fields fit on a single line?
                        (or (null? fields)
                            (short-width . < . max-proto-width))]
                       [split-field-line?
                        ;; start fields on the line after "struct"?
                        (and (pair? fields)
                             (max-proto-width . < . (+ 8
                                                       (if (pair? name)
                                                           (+ (sym-length (car name))
                                                              1
                                                              (sym-length (cadr name)))
                                                           (sym-length name))
                                                       1
                                                       (sym-length (field-name (car fields)))
                                                       1)))])
                  (make-table
                   #f
                   ;; First four columns: "(struct" <space> <name><space> (
                   ;;   If all fields on the first line, extra columns follow;
                   ;;   If only first field on same line, filds are in fourth column
                   ;;   If no field is on the first line, no fourth column after all
                   ;;    and fields are in the second column
                   (append
                    (list
                     (append
                      (list (to-flow (make-element #f 
                                                   (list
                                                    (racketparenfont "(")
                                                    (racket struct))))
                            flow-spacer)
                      (if one-right-column?
                          ;; struct name and fields on one line:
                          (list (to-flow (list the-name
                                               spacer
                                               (to-element (map field-view
                                                                fields))
                                               (if (and immutable?
                                                        (not transparent?)
                                                        (not cname-id))
                                                   (racketparenfont ")")
                                                   null))))
                          (if split-field-line?
                              ;; Field start on line after "struct":
                              (list (to-flow (make-element 'no-break the-name)))
                              ;; First field on the same line as "struct":
                              (list (to-flow (make-element 'no-break the-name))
                                    (to-flow (make-element
                                              #f (list spacer (racketparenfont "("))))
                                    (to-flow (make-element 'no-break
                                                           (let ([f (to-element (field-view (car fields)))])
                                                             (if (null? (cdr fields))
                                                                 (list f (racketparenfont ")"))
                                                                 f)))))))))
                    (if split-field-line?
                        ;; First field, which starts on the next line:
                        (list
                         (list flow-spacer flow-spacer
                               (to-flow (list
                                         (racketparenfont "(")
                                         (make-element 'no-break 
                                                       (let ([f (to-element (field-view (car fields)))])
                                                         (if (null? (cdr fields))
                                                             (list f (racketparenfont ")"))
                                                             f)))))))
                        null)
                    ;; Remaining fields:
                    (if one-right-column?
                        null
                        (let loop ([fields (if (null? fields)
                                               fields
                                               (cdr fields))])
                          (if (null? fields)
                              null
                              (cons
                               (let ([fld (car fields)])
                                 (append
                                  (list flow-spacer flow-spacer)
                                  (if split-field-line? null (list flow-spacer flow-spacer))
                                  (list (to-flow
                                         (list
                                          (if split-field-line? spacer null)
                                          (let ([e (to-element (field-view fld))])
                                            (if (null? (cdr fields))
                                                (list e
                                                      (racketparenfont
                                                       (if (and immutable?
                                                                (not transparent?)
                                                                (not cname-id))
                                                           "))" 
                                                           ")")))
                                                e)))))))
                               (loop (cdr fields)))))))))))))))
      ;; Next lines at "boxed" level are construct-name keywords:
      (if cname-id
          (let ([kw (to-element (if (if cname-given?
                                        extra-cname?
                                        default-extra?)
                                    '#:extra-constructor-name
                                    '#:constructor-name))]
                [nm (to-element cname-id)]
                [close? (and immutable?
                             (not transparent?))])
            (if (max-proto-width . < . (+ (element-width keyword-spacer)
                                          1 ; space between kw & name
                                          (element-width kw) 
                                          (element-width nm)
                                          (if close? 1 0)))
                ;; use two lines for #:constructor-name
                (list (list (to-flow (list keyword-spacer kw)))
                      (list (to-flow
                             (list
                              keyword-spacer
                              (if close?
                                  (make-element #f (list nm (racketparenfont ")")))
                                  nm)))))
                ;; use one line for #:constructor-name
                (list (list 
                       (to-flow (make-element 
                                 #f
                                 (list
                                  keyword-spacer
                                  kw (hspace 1) nm
                                  (if close?
                                      (racketparenfont ")")
                                      null))))))))
          null)
      ;; Next lines at "boxed" level are prefab/transparent/mutable
      (cond
       [(and (not immutable?) transparent?)
        (list
         (list (to-flow (list keyword-spacer (to-element '#:mutable))))
         (list (to-flow (list keyword-spacer
                              (if prefab?
                                  (to-element '#:prefab)
                                  (to-element '#:transparent))
                              (racketparenfont ")")))))]
       [(not immutable?)
        (list
         (list (to-flow (list keyword-spacer
                              (to-element '#:mutable)
                              (racketparenfont ")")))))]
       [transparent?
        (list
         (list (to-flow (list keyword-spacer
                              (if prefab?
                                  (to-element '#:prefab)
                                  (to-element '#:transparent))
                              (racketparenfont ")")))))]
       [else null])
      ;; Remaining lines at "boxed" level are field contracts:
      (map (lambda (v field-contract)
             (cond
              [(pair? v)
               (list
                (top-align
                 make-table-if-necessary
                 "argcontract"
                 (list (list (to-flow (hspace 2))
                             (to-flow (to-element (field-name v)))
                             flow-spacer
                             (to-flow ":")
                             flow-spacer
                             (make-flow (list (field-contract)))))))]
              [else null]))
           fields field-contracts))))
  (make-box-splice
   (cons
    (make-blockquote
     vertical-inset-style
     (list main-table))
    (content-thunk))))

;; ----------------------------------------

(define-syntax (defthing stx)
  (syntax-parse stx
    [(_ kind:kind-kw 
        lt:link-target?-kw 
        (~optional (~seq #:id id-expr)
                   #:defaults ([id-expr #'#f]))
        id 
        result 
        value:value-kw
        desc ...)
     #'(with-togetherable-racket-variables
        ()
        ()
        (let ([id-val id-expr])
          (*defthing kind.kind
                     lt.expr
                     (list (or id-val (quote-syntax/loc id))) (list (if (identifier? id-val) (syntax-e id-val) 'id)) #f
                     (list (racketblock0 result))
                     (lambda () (list desc ...))
                     (list (result-value value.value)))))]))

(define-syntax (defthing* stx)
  (syntax-parse stx
    [(_ kind:kind-kw lt:link-target?-kw ([id result value:value-kw] ...) desc ...)
     #'(with-togetherable-racket-variables
        ()
        ()
        (*defthing kind.kind
                   lt.expr
                   (list (quote-syntax/loc id) ...) (list 'id ...) #f
                   (list (racketblock0 result) ...)
                   (lambda () (list desc ...))
                   (list (result-value value.value) ...)))]))

(define (*defthing kind link? stx-ids names form? result-contracts content-thunk
                   [result-values (map (lambda (x) #f) result-contracts)])
  (define max-proto-width (current-display-width))
  (make-box-splice
   (cons
    (make-blockquote
     vertical-inset-style
     (list
      (make-table
       boxed-style
       (append*
        (for/list ([stx-id (in-list stx-ids)]
                   [name (in-list names)]
                   [result-contract (in-list result-contracts)]
                   [result-value (in-list result-values)]
                   [i (in-naturals)])
          (let* ([result-block
                  (and result-value
                       (if (block? result-value)
                           result-value
                           (make-omitable-paragraph (list result-value))))]
                 [contract-block
                  (if (block? result-contract)
                      result-contract
                      (make-omitable-paragraph (list result-contract)))]
                 [name+contract-width (+ (string-length (format "~a" name))
                                         3
                                         (block-width contract-block))]
                 [total-width (+ name+contract-width
                                 (if result-block
                                     (+ (block-width result-block) 3)
                                     0))]
                 [thing-id (let ([target-maker
                                  (and link?
                                       ((if form? id-to-form-target-maker id-to-target-maker)
                                        stx-id #t))])
                             (define-values (content ref-content) 
                               (if link?
                                   (definition-site name stx-id form?)
                                   (let ([s (make-just-context name stx-id)])
                                     (values (to-element #:defn? #t s)
                                             (to-element s)))))
                             (if target-maker
                                 (target-maker
                                  content
                                  (lambda (tag)
                                    (make-toc-target2-element
                                     #f
                                     (make-index-element
                                      #f
                                      content
                                      tag
                                      (list (datum-intern-literal (symbol->string name)))
                                      (list ref-content)
                                      (with-exporting-libraries
                                       (lambda (libs) (make-thing-index-desc name libs))))
                                     tag
                                     ref-content)))
                                 content))]
                 [contract-on-first-line? (name+contract-width . < . max-proto-width)]
                 [single-line? (and contract-on-first-line?
                                    (total-width . < . max-proto-width)
                                    (not (table? result-value)))])
            (append
             (list
              (list
               ((if (zero? i) (add-background-label (or kind "value")) values)
                (top-align
                 make-table-if-necessary
                 "argcontract"
                 (append
                  (list
                   (append
                    (list (list (make-omitable-paragraph
                                 (list thing-id))))
                    (if contract-on-first-line?
                        (list
                         (to-flow (list spacer ":" spacer))
                         (list contract-block))
                        null)
                    (if (and result-block single-line?)
                        (list
                         (to-flow (list spacer "=" spacer))
                         (list result-block))
                        null))))))))
             (if contract-on-first-line?
                 null
                 (list (list (top-align
                              make-table-if-necessary
                              "argcontract"
                              (list 
                               (list (to-flow (list spacer ":" spacer))
                                     (list contract-block)))))))
             (if (or single-line? (not result-block))
                 null
                 (list (list (top-align
                              make-table-if-necessary
                              "argcontract"
                              (list (list
                                     (to-flow (list spacer "=" spacer))
                                     (list result-block))))))))))))))
     (content-thunk))))

(define (defthing/proc kind id contract descs)
  (*defthing kind #t (list id) (list (syntax-e id)) #f (list contract)
             (lambda () descs)))

(define (make-target-element* inner-make-target-element stx-id content wrappers)
  (if (null? wrappers)
    content
    (make-target-element*
     make-target-element
     stx-id
     (let* ([name (datum-intern-literal (string-append* (map symbol->string (cdar wrappers))))]
            [target-maker
             (id-to-target-maker (datum->syntax stx-id (string->symbol name))
                                 #t)])
       (if target-maker
         (target-maker
          content
          (lambda (tag)
            (inner-make-target-element
             #f
             (make-index-element
              #f
              content
              tag
              (list name)
              (list (racketidfont (make-element value-link-color
                                                (list name))))
              (with-exporting-libraries
               (lambda (libs)
                 (let ([name (string->symbol name)])
                   (if (eq? 'info (caar wrappers))
                       (make-struct-index-desc name libs)
                       (make-procedure-index-desc name libs))))))
             tag)))
         content))
     (cdr wrappers))))


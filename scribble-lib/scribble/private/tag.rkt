#lang scheme/base

;; It might make sense to make these functions public, but since they weren't originally,
;; I am going to keep them in the private folder for now.
;; -- With Love, Leif

(provide (all-defined-out))

(require racket/list
         scribble/core
         "../tag.rkt")

(define (gen-tag content)
  (datum-intern-literal
   ;; Generate tag from ASCII plus CJK characters. Constraining to
   ;; ASCII for most purposes helps avoid encoding issues for
   ;; uncooperative environments, but constraining to ASCII is too
   ;; uncooperative in another direction for CJK text (i.e., creates
   ;; too many conflicting tags).
   (regexp-replace* #px"[^-a-zA-Z0-9_=\u4e00-\u9fff\u3040-\u309F\u30A0-\u30FF]"
                    (content->string content) "_")))

(define (convert-tag tag content)
  (if (list? tag)
    (append-map (lambda (t) (convert-tag t content)) tag)
    `((part ,(or tag (gen-tag content))))))

(define (convert-part-style who s)
  (cond
   [(style? s) s]
   [(not s) plain]
   [(string? s) (make-style s null)]
   [(symbol? s) (make-style #f (list s))]
   [(and (list? s) (andmap symbol? s)) (make-style #f s)]
   [else (raise-argument-error who "(or/c style? string? symbol? (listof symbol?) #f)" s)]))

(define (prefix->string p)
  (and p (if (string? p) 
             (datum-intern-literal p)
             (module-path-prefix->string p))))

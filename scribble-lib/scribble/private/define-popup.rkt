#lang racket/base
(require racket/class)
(provide define-popup)

(define (get-text-enclosed-in-curlys txt pos def)
  (let loop ([pos pos]
             [found-open? #f]
             [chars '()])
    (cond
      [(< pos (send txt last-position))
       (define char (send txt get-character pos))
       (cond
         [found-open?
          (cond
            [(char=? char #\})
             (regexp-replace
              #rx"^[\n ]*"
              (regexp-replace
               #rx"[\n ]*$"
               (apply string (reverse chars))
               "")
              "")]
            [else
             (loop (+ pos 1) #t (cons char chars))])]
         [else
          (cond
            [(char=? char #\{)
             (loop (+ pos 1) #t '())]
            [else
             (loop (+ pos 1) #f '())])])]
      [else #f])))

(define define-popup
  (list (list "@section" "Sections" "Sec" #f
              get-text-enclosed-in-curlys)
        (list "@title" "Sections" "Sec" #f
              get-text-enclosed-in-curlys)))

(module+ test
  (require rackunit)

  (define fake-text%
    (class object%
      (init-field str)
      (define/public (last-position) (string-length str))
      (define/public (get-character i) (string-ref str i))
      (super-new)))
  
  (let ()
    (define t (new fake-text% [str "@section{abc}"]))
    (check-equal? (get-text-enclosed-in-curlys t 8 void) "abc"))

  (let ()
    ;; technically, the space here should disqualify this as a section,
    ;; but we'll go with it for now to make it easier handle the test
    ;; case below with @section[...]
    (define t (new fake-text% [str "@section {abc}"]))
    (check-equal? (get-text-enclosed-in-curlys t 8 void) "abc"))

  (let ()
    (define t (new fake-text% [str "@sectionabc"]))
    (check-equal? (get-text-enclosed-in-curlys t 8 void) #f))

  (let ()
    (define t (new fake-text% [str "@section{\n abc\n }"]))
    (check-equal? (get-text-enclosed-in-curlys t 8 void) "abc"))

  (let ()
    (define t (new fake-text% [str "@section[#:tag \"sec:abc\"]{abc}"]))
    (check-equal? (get-text-enclosed-in-curlys t 8 void) "abc")))

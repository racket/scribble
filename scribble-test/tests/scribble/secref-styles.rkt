#lang racket/base

;; Tests for the link-render-style modes ('default, 'number, 'short, and
;; 'number-and-title; see scribble/core's link-element and
;; link-render-style docs), across HTML and LaTeX output.
;;
;; Checks use the exact literal text each mode is expected to produce
;; (via regexp-quote, to avoid hand-escaping mistakes), rather than loose
;; substring matches, since this document's own section headings share
;; words (and even the whole title) with what a secref renders -- a loose
;; match on, say, the title text alone would pass even if the secref
;; itself rendered nothing at all.

(require racket/class
         racket/file
         racket/runtime-path
         rackunit
         scribble/base-render
         (prefix-in html: scribble/html-render)
         (prefix-in latex: scribble/latex-render))

(define-runtime-path secref-styles-scrbl "docs/secref-styles.scrbl")
(define work-dir (build-path (find-system-path 'temp-dir)
                              "scribble-secref-styles-tests"))

(define (build-doc render% dest-file)
  (define renderer (new render% [dest-dir work-dir]))
  (define docs
    (list (if (module-declared? `(submod ,secref-styles-scrbl doc) #t)
              (dynamic-require `(submod ,secref-styles-scrbl doc) 'doc)
              (dynamic-require secref-styles-scrbl 'doc))))
  (define fns (list (build-path work-dir dest-file)))
  (define fp (send renderer traverse docs fns))
  (let ([docs (send renderer traversed-parts docs fp)])
    (define info (send renderer collect docs fns fp))
    (define r-info (send renderer resolve docs fns info))
    (send renderer render docs fns r-info)
    (void)))

(define (has? out str)
  (regexp-match? (regexp-quote str) out))

(provide secref-styles-tests)
(module+ main (secref-styles-tests))
(module+ test (secref-styles-tests))

(define (secref-styles-tests)
  (when (or (file-exists? work-dir) (directory-exists? work-dir))
    (delete-directory/files work-dir))
  (dynamic-wind
    (λ () (make-directory work-dir))
    (λ ()
      (check-not-exn
       (λ () (build-doc (html:render-mixin render%) "secref-styles.html")))
      (define html-out (file->string (build-path work-dir "secref-styles.html")))

      ;; 'default: the title wrapped in an anchor tag -- distinguishing
      ;; this from the identical text in the section's own (unwrapped)
      ;; heading.
      (check-true (regexp-match? #rx"<a [^>]*>A Numbered Section</a>" html-out))
      ;; 'number: the word "section" (this document uses no #:uppercase
      ;; style), followed by the hyperlinked number "1" and a closing
      ;; anchor tag -- distinguishing this from the unrelated word
      ;; "section" in this file's own prose, which is never followed by
      ;; an anchor tag.
      (check-true (regexp-match? #rx"section [^<]*<a [^>]*>1</a>" html-out))
      ;; 'short: just the section number, as "§1", hyperlinked.
      (check-true (has? html-out "§1"))
      ;; 'short falls back to the plain title, still hyperlinked (as
      ;; opposed to the section's own unwrapped heading), when unnumbered.
      (check-true (regexp-match? #rx"<a [^>]*>An Unnumbered Section</a>" html-out))
      ;; 'number-and-title: the number and the quoted title together.
      (check-true (has? html-out "§1 “A Numbered Section”"))
      ;; 'number-and-title falls back to just the quoted title (still
      ;; distinguishable from the plain heading) when unnumbered.
      (check-true (has? html-out "“An Unnumbered Section”"))
      ;; A target with no numbering metadata at all (dest-number is #f,
      ;; not just an empty list, e.g. for a bare target-element that
      ;; isn't a section) should render as just the quoted title, like
      ;; the unnumbered-section case above.
      (check-true (has? html-out "“A Bare Target”"))

      ;; This also exercises 'short on an unnumbered section with a
      ;; genuinely empty title, which render-self-contained-secref must
      ;; not mistake for another empty-content part-label link (that
      ;; would re-enter the same method indefinitely). There isn't much
      ;; else to meaningfully assert about a link with an empty title,
      ;; so this check-not-exn is the test for it.
      (check-not-exn
       (λ () (build-doc (latex:render-mixin render%) "secref-styles.tex")))
      (define tex-out (file->string (build-path work-dir "secref-styles.tex")))

      ;; 'default and 'number depend on document-style .tex macros (see
      ;; scribble.tex/manual-style.tex) that only expand into their final
      ;; wording when actually compiled with a LaTeX toolchain, which this
      ;; test doesn't do -- so only a smoke check (above) applies to them
      ;; for LaTeX; 'short and 'number-and-title, by contrast, write their
      ;; final literal text directly, so their exact output can be checked
      ;; here without compiling.

      ;; 'short: the \S command (escaped as {\S}) followed by the number.
      (check-true (has? tex-out "{\\S}1"))
      ;; 'number-and-title: the number, then the quoted title (curly
      ;; quotes are escaped as {``}/{''}, per convert-to-latex).
      (check-true (has? tex-out "{\\S}1 {``}A Numbered Section{''}"))
      ;; 'number-and-title falls back to just the quoted title when
      ;; unnumbered.
      (check-true (has? tex-out "{``}An Unnumbered Section{''}"))
      ;; Same no-numbering-metadata case as above, for LaTeX.
      (check-true (has? tex-out "{``}A Bare Target{''}"))
      ;; The self-contained 'short/'number-and-title path must preserve
      ;; the original link's style (e.g. color), not just its content:
      ;; \intextcolor{red}{...} should wrap the "{\S}1" content.
      (check-true (has? tex-out "\\intextcolor{red}{{\\S}1}"))
      (void))
    (λ () (delete-directory/files work-dir))))

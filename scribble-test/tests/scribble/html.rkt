#lang racket/base

;; Check that the HTML tags provided by scribble/html/html
;;  and scribble/html/extra
;;  match a master list of HTML tags (defined in this file)
;; Also check that `scribble/html/html` is disjoint from `racket/base`

(require rackunit racket/set)

(define (phase0-provides m) ; Symbol -> [Setof Symbol]
  (parameterize ([current-namespace (make-base-namespace)])
    (dynamic-require m #f)
    (let-values ([(e1* e2*) (module->exports m)])
      (for*/seteq ([export* (in-list (list e1* e2*))]
                   [tag+exp (in-list export*)]
                   #:when (or (not (car tag+exp)) (zero? (car tag+exp)))
                   [exp (in-list (cdr tag+exp))])
        (car exp)))))

(define (expected-disjoint m1 m2)
  (format "'~a' and '~a' should provide disjoint sets of identifiers" m1 m2))

(define (expected-overlap m1 m2)
  (format "expected '~a' and '~a' to provide overlapping sets of identifiers" m1 m2))

(define html-provides (phase0-provides 'scribble/html/html))
(define extra-provides (phase0-provides 'scribble/html/extra))
(define base-provides (phase0-provides 'racket/base))

(check-pred set-empty?
            (set-intersect html-provides extra-provides)
            (expected-disjoint 'scribble/html/html 'scribble/html/extra))

;; note: 'racket' and 'scribble/html/html' both provide "link"
(check-pred set-empty?
            (set-intersect html-provides base-provides)
            (expected-disjoint 'scribble/html/html 'racket/base))

(check-pred positive?
            (set-count (set-intersect extra-provides base-provides))
            (expected-overlap 'scribble/html/extra 'racket/base))

;; From: https://html.spec.whatwg.org/multipage/#toc-semantics
(define whatwg-master (list->seteq '(
  html
  head
  title
  base
  link
  meta
  style
  body
  article
  section
  nav
  aside
  h1 h2 h3 h4 h5 h6
  hgroup
  header
  footer
  address
  p
  hr
  pre
  blockquote
  ol
  ul
  li
  dl
  dt
  dd
  figure
  figcaption
  main
  div
  a
  em
  strong
  small
  s
  cite
  q
  dfn
  abbr
  ruby
  rt
  rp
  data
  time
  code
  var
  samp
  kbd
  sub sup
  i
  b
  u
  mark
  bdi
  bdo
  span
  br
  wbr
  ins
  del
  picture
  source
  img
  iframe
  embed
  object
  param
  video
  audio
  track
  map
  area
  table
  caption
  colgroup
  col
  tbody
  thead
  tfoot
  tr
  td
  th
  form
  label
  input
  button
  select
  datalist
  optgroup
  option
  textarea
  keygen
  output
  progress
  meter
  fieldset
  legend
  details
  summary
  menu
  menuitem
  dialog
  script
  noscript
  template
  slot
  canvas)))

(let ([scribble-master (set-union html-provides extra-provides)])
  (check-true (subset? whatwg-master scribble-master))
  ;; Uncomment to debug scribble provides vs. whatwg master
  #;(void
    (displayln "checking MASTER vs SCRIBBLE")
    (for ([m (in-set whatwg-master)]
          #:when (not (set-member? scribble-master m)))
      (displayln m))
    (newline)
    (displayln "checking SCRIBBLE vs MASTER")
    (for ([s (in-set scribble-master)]
          #:when (not (set-member? whatwg-master s)))
      (displayln s))))

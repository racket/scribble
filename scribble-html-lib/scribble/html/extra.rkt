#lang racket/base

;; (X)HTML elements that are uncommon / likely to cause namespace conflicts

(require "xml.rkt")

(define/provide-elements/empty
  source
  track)

(define/provide-elements/not-empty
  article
  aside
  audio
  bdi
  canvas
  data
  datalist
  figcaption
  figure
  footer
  header
  main
  map
  mark
  math
  meter
  nav
  output
  picture
  progress
  rb
  rp
  rt
  rtc
  ruby
  section
  summary
  svg
  template
  time
  video)

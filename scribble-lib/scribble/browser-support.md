## Scribble Browser Support Policy

This document describes the intended browser support range for HTML
manuals generated with `#lang scribble/manual`.

This policy defines three levels of support:

- Full support (all features). 
- Partial support (documentation features)
- Minimal support (reading documentation text)

Any feature must work correctly in all browsers with full support.

Features that are not needed for reading and understanding the
documentation, but that are intended for developers of the
documentation or other specialized audiences, such as links to
implementation, may not work in browsers with only partial support.

For browsers with minimal support, it will be possible to read the
text of the documentation, but all other features (such as proper
styling or links) may be missing.

#### Currently supported browsers

- All "evergreen" browsers current within the past 12 months (this
  includes Chrome, Firefox, Safari, Edge, and other regularly-updated
  browsers based on similar). "Current" means if that browser was
  shipped to users within the past 12 months, it is fully supported.
  
- Partial support includes any browser released within the past 5
  years with at least 1% use on https://docs.racket-lang.org As of
  June 1, 2020, that includes the above browsers only.
  
  Note that the only browser not in the above set with more than 1%
  share is Chrome 38 as distributed on the Nexus 5, which was
  discontinued in March 2015.
  
- Minimal support applies to all widely-distributed browsers. That is,
  it is important that the text of the documentation appears in Links,
  IE 6, Opera Mini, etc.
    

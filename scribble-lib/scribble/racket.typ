#let SColorizeOn = state("colorize", true)

#let Stext(fill: luma(0%), style: "normal", body) = {
  context {
    if SColorizeOn.get() {
      text(fill: fill, style: style, body)
    } else {
      text(style: style, body)
    }
  }
}

#let Shighlight(fill: none, body) = {
  context {
    if SColorizeOn.get() {
      highlight(fill: fill, body)
    } else {
      body
    }
  }
}

// Style definitions for rendering Racket code in Typst output,
// following the colors of "racket.css". This file is included after
// the prefix file and before any style files, so a style file (as
// supplied with the `--style` or `++style` flags) can shadow these
// definitions with new `#let` bindings.
#let RktPlain(body) = Stt(body)
#let RktIn(body) = Shighlight(fill: rgb("#eeeeee"), Stt(Stext(fill: rgb("#cc6633"), body)))
#let RktInBG(body) = Shighlight(fill: rgb("#eeeeee"), body)
#let RktRdr(body) = Stt(body)
#let RktPn(body) = Stt(Stext(fill: rgb("#843c24"), body))
#let RktMeta(body) = Stt(body)
#let RktMod(body) = Stt(body)
#let RktKw(body) = Stt(body)
#let RktOpt(body) = Stt(Stext(style: "italic", body))
#let RktErr(body) = Stext(fill: red, style: "italic", body)
#let RktVar(body) = Stt(Stext(fill: rgb("#262680"), style: "italic", body))
#let RktSym(body) = Stt(Stext(fill: rgb("#262680"), body))
#let RktSymDef(body) = RktSym(body)
#let RktValLink(body) = Stt(Stext(fill: rgb("#0000ff"), body))
#let RktValDef(body) = RktValLink(body)
#let RktModLink(body) = Stt(Stext(fill: rgb("#0000ff"), body))
#let RktStxLink(body) = body
#let RktStxDef(body) = RktStxLink(body)
#let RktRes(body) = Stt(Stext(fill: rgb("#0000af"), body))
#let RktOut(body) = Stt(Stext(fill: rgb("#960096"), body))
#let RktCmt(body) = Stt(Stext(fill: rgb("#c2741f"), body))
#let RktVal(body) = Stt(Stext(fill: rgb("#228b22"), body))
#let highlighted(body) = Shighlight(fill: rgb("#ddddff"), body)

#let RktBlk(columns: [], ..content) = Stable(columns: columns, ..content)

#let RpackageSpec(e) = [#h(1fr) #e]
#let RBackgroundLabel(body) = []
#let RBackgroundLabelInner(body) = body

// For `filebox`:
#let Rfilebox(title, body) = body
#let Rfiletitle(body) = align(right, body)
#let Rfilename(body) = box(fill: rgb("#ECF5F5"),
                           stroke: (top: 0.6pt + rgb("#6C8585"), right: 0.6pt + rgb("#6C8585")),
                           inset: (x: 0.5em, y: 2pt),
                           body)
#let Rfilecontent(body) = body

// For inherited-method labels in `defclass`:
#let inheritedlbl(body) = text(style: "italic", body)

#let defmodule(columns: [], ..content) = block(width: 100%,
                                               fill: rgb("#F5F5DC"),
                                               inset: 3pt,
                                               Stable(columns: columns, ..content))
#let prototype = Stable
#let together = Stable
#let specgrammar = Stable
#let argcontract = Stable
#let RBibliography = Stable

#let leftindent = SInset
#let insetpara = SInset

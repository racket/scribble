#lang scribble/base
@(require scriblib/render-cond
          (only-in scribble/core
                   part
                   plain
                   paragraph))

@(cond-element
  [text "Text!"]
  [html "HTML!"]
  [latex "Latex!"]
  [markdown "Markdown!"]
  [typst "Typst!"])

@(cond-element
  [(or text html) "Text or HTML!"]
  [else "Latex!"])

@(cond-element
  [(and text html) "Text and HTML?!"]
  [else "Other!"])

@(cond-element
  [(not text) "Not Text!"]
  [else "Text!"])

@(cond-part
  [text (list (part #f null (list "Extra Text Part") plain null
                    (list (paragraph plain (list "This part is text-only.")))
                    null))]
  [else null])

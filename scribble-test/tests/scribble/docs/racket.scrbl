#lang scribble/manual

@; The tests don't check the styling and indentation. I inspected
@; those manually. 

@;%
@(begin
(racketblock
 (code:comment2 @#,elem{{f : number -> number}})
 (code:comment# @#,elem{f : number -> number})
 (code:contract# {f : number -> number})
))
@;%

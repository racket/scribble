#lang scribble/base

@;{ This test is mainly intended for checking Latex
    rendering of box characters, but it should render
    ok with text, too. }

@(define charss
         '((#\┌ #\─ #\┐ #\│ #\└ #\┘ #\┬ #\├ #\┤ #\┼ #\┴)
           (#\╔ #\═ #\╗ #\║ #\╚ #\╝ #\╦ #\╠ #\╣ #\╬ #\╩)
           (#\┏ #\━ #\┓ #\┃ #\┗ #\┛ #\┳ #\┣ #\┫ #\╋ #\┻)))

@(define (adj-verbatim which . strs)
  (define ht (for/hash ([c1 (in-list (car charss))]
                        [c2 (in-list (list-ref charss which))])
               (values c1 c2)))
  (apply verbatim
         (for/list ([s (in-list strs)])
           (list->string
            (for/list ([c (in-string s)])
              (hash-ref ht c c))))))

@verbatim{
┌┬─┐
├┤ │
└┼─┼┐
 └─┴┘
 }

@adj-verbatim[1]{
┌┬─┐
├┤ │
└┼─┼┐
 └─┴┘
 }

@adj-verbatim[2]{
┌┬─┐
├┤ │
└┼─┼┐
 └─┴┘
 }

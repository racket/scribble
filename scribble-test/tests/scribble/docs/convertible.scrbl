#lang scribble/base
@(require file/convertible)

@(struct s ()
   #:property
   prop:convertible
   ;; Not actually convertible to anything:
   (lambda (v req default)
     default))

@(struct c ()
   #:property
   prop:convertible
   (lambda (v req default)
     (cond
       [(eq? req 'text) "hello"]
       [else default])))

@(s)

@(c)

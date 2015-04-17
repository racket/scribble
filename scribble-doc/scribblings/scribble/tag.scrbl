#lang scribble/doc
@(require scribble/manual "utils.rkt"
          (for-label setup/main-collects))

@title[#:tag "tag"]{Tag Utilities}

@declare-exporting[scribble/tag scribble/base]

@defmodule*/no-declare[(scribble/tag)]{The @racketmodname[scribble/tag]
library provides utilities for constructing cross-reference
@tech{tags}. The library is re-exported by
@racketmodname[scribble/base].}

@; ------------------------------------------------------------------------

@defproc[(make-section-tag [name string?]
                           [#:doc doc-mod-path (or/c module-path? #f) #f]
                           [#:tag-prefixes tag-prefixes (or/c #f (listof string?)) #f])
         tag?]{

Forms a @tech{tag} that refers to a section whose ``tag'' (as provided
by the @racket[#:tag] argument to @racket[section], for example) is
@racket[name]. If @racket[doc-mod-path] is provided, the @tech{tag}
references a section in the document implemented by
@racket[doc-mod-path] from outside the document. Additional tag
prefixes (for intermediate sections, typically) can be provided as
@racket[tag-prefixes].}

@defproc[(make-module-language-tag [lang symbol?]) tag?]{
  Forms a @tech{tag} that refers to a section 
  that contains @racket[defmodulelang] for the language
  @racket[lang].
}

@defproc[(taglet? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{taglet}, @racket[#f]
otherwise.

A @deftech{taglet} is a value that can be combined with a symbol via
@racket[list] to form a @tech{tag}, but that is not a
@racket[generated-tag]. A @tech{taglet} is therefore useful as a piece
of a @tech{tag}, and specifically as a piece of a tag that can gain a
prefix (e.g., to refer to a section of a document from outside the
document).}


@defproc*[([(doc-prefix [mod-path (or/c #f module-path?)] 
                        [taglet taglet?])
            taglet?]
           [(doc-prefix [mod-path (or/c #f module-path?)]
                        [extra-prefixes (or/c #f (listof taglet?))]
                        [taglet taglet?])
            taglet?])]{

Converts part of a cross-reference @tech{tag} that would work within a
document implemented by @racket[mod-path] to one that works from
outside the document, assuming that @racket[mod-path] is not
@racket[#f]. That is, @racket[mod-path] is converted to a
@tech{taglet} and added as prefix to an existing @racket[taglet].

If @racket[extra-prefixes] is provided, then its content is added as a
extra prefix elements before the prefix for @racket[mod-path] is
added. A @racket[#f] value for @racket[extra-prefixes] is equivalent
to @racket['()].

If @racket[mod-path] is @racket[#f], then @racket[taglet] is returned
without a prefix (except adding @racket[extra-prefixes], if provided).}


@defproc[(module-path-prefix->string [mod-path module-path?]) string?]{

Converts a module path to a string by resolving it to a path, and
using @racket[path->main-collects-relative].}

@defproc[(module-path-index->taglet [mpi module-path-index?]) taglet?]{

Converts a module path index to a @tech{taglet}---a normalized
encoding of the path as an S-expression---that is interned via
@racket[intern-taglet].

The string form of the @tech{taglet} is used as prefix in a @tech{tag}
to form cross-references into the document that is implemented by the
module referenced by @racket[mpi].}

@defproc[(intern-taglet [v any/c]) any/c]{

Returns a value that is @racket[equal?] to @racket[v], where multiple
calls to @racket[intern-taglet] for @racket[equal?] @racket[v]s
produce the same (i.e., @racket[eq?]) value.}


@defproc[(definition-tag->class/interface-tag [definition-tag definition-tag?])
         class/interface-tag?]{
  Constructs a tag like @racket[definition-tag], except that
  it matches documentation for the class. If @racket[definition-tag]
  doesn't document a class or interface, this function still returns
  the tag that the class or interface documentation would have had,
  as if @racket[definition-tag] had documented a class or interface.

  @history[#:added "1.11"]
}
@defproc[(class/interface-tag->constructor-tag [class/interface-tag class/interface-tag?])
         constructor-tag?]{
  Constructs a tag like @racket[definition-tag], except that
  it matches documentation for the constructor of the class.

  @history[#:added "1.11"]
}
@defproc[(get-class/interface-and-method [method-tag method-tag?])
         (values symbol? symbol?)]{
  Returns the class name and method name (respectively) for the method documented
  by the docs at @racket[method-tag].

  @history[#:added "1.11"]
}
@defproc[(definition-tag? [v any/c]) boolean?]{
 Recognizes definition tags. If @racket[(definition-tag? _v)] is
 @racket[#t], then so is @racket[(tag? _v)].

  @history[#:added "1.11"]
}                               
@defproc[(class/interface-tag? [v any/c]) boolean?]{
 Recognizes class or interface tags. If @racket[(class/interface-tag? _v)] is
 @racket[#t], then so is @racket[(tag? _v)].

  @history[#:added "1.11"]
}
@defproc[(method-tag? [v any/c]) boolean?]{
 Recognizes method tags. If @racket[(method-tag? _v)] is
 @racket[#t], then so is @racket[(tag? _v)].

  @history[#:added "1.11"]
}
@defproc[(constructor-tag? [v any/c]) boolean?]{
 Recognizes class constructor tags. If @racket[(constructor-tag? _v)] is
 @racket[#t], then so is @racket[(tag? _v)].

  @history[#:added "1.11"]
}

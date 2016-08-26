#lang scribble/manual

@(require scribble/eval)

@(require (for-label racket/base
                     racket/match
                     kw-make-struct))

@title{kw-make-struct}

@defmodule[kw-make-struct]

source code: @url["https://github.com/AlexKnauth/kw-make-struct"]

@(define make-from-unstable/struct
   "https://docs.racket-lang.org/unstable/struct.html#%28form._%28%28lib._unstable%2Fstruct..rkt%29._make%29%29")

@defform*[[(make/kw struct-id field ...)
           (make/kw struct-id field-pat ...)]
          #:grammar ([field (code:line expr)
                            (code:line field-keyword expr)]
                     [field-pat (code:line pat)
                                (code:line field-keyword pat)])]{
Like @hyperlink[make-from-unstable/struct]{@racket[make] from @racketmodfont{unstable/struct}},
except allowing keywords.  

@racket[make/kw] is also defined as a @racket[match] expander.

@examples[
  (require kw-make-struct racket/match)
  (struct foo (a b c) #:transparent)
  (make/kw foo 'a 'b 'c)
  (make/kw foo #:a 'a #:b 'b #:c 'c)
  (make/kw foo #:a 'a 'b 'c)
  (make/kw foo #:c 'c 'a #:b 'b)
  (match (foo 'a 'b 'c)
    [(make/kw foo #:a a #:b b #:c c)
     (list a b c)])
]}

@defform*[[(make/fld struct-id [field-id expr] ...)
           (make/fld struct-id [field-id pat] ...)]]{
Creates an instance of @racket[struct-id], where @racket[[field-id expr]]
means that the @racket[field-id] field will be the value of @racket[expr].

@racket[make/fld] is also defined as a @racket[match] expander.

@examples[
  (require kw-make-struct racket/match)
  (struct foo (a b c) #:transparent)
  (make/fld foo [a 'a] [b 'b] [c 'c])
  (make/fld foo [c 'c] [a 'a] [b 'b])
  (match (foo 'a 'b 'c)
    [(make/fld foo [a a] [b b] [c c])
     (list a b c)])
]}


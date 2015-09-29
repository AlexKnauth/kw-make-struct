#lang scribble/manual

@(require scribble/eval)

@(require (for-label racket/base
                     racket/match
                     unstable/struct
                     kw-make-struct))

@title{kw-make-struct}

@defmodule[kw-make-struct]

source code: @url["https://github.com/AlexKnauth/kw-make-struct"]

@defform*[[(make/kw struct-id field ...)
           (make/kw struct-id field-pat ...)]
          #:grammar ([field (code:line expr)
                            (code:line field-keyword expr)]
                     [field-pat (code:line pat)
                                (code:line field-keyword pat)])]{
like @racket[make] from @racketmodname[unstable/struct], except allowing keywords.  

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


kw-make-struct
==============

[![Build Status](https://travis-ci.org/AlexKnauth/kw-make-struct.png?branch=master)](https://travis-ci.org/AlexKnauth/kw-make-struct)

documentation: http://pkg-build.racket-lang.org/doc/kw-make-struct/index.html

like [make](http://docs.racket-lang.org/unstable/struct.html#%28form._%28%28lib._unstable%2Fstruct..rkt%29._make%29%29) from unstable/struct except allowing keywords

`make/kw` is also defined as a match-expander.

Examples:
```racket
> (struct foo (a b c) #:transparent)
> (make/kw foo 'a 'b 'c)
(foo 'a 'b 'c)
> (make/kw foo #:a 'a #:b 'b #:c 'c)
(foo 'a 'b 'c)
> (make/kw foo #:a 'a 'b 'c)
(foo 'a 'b 'c)
> (make/kw foo #:c 'c 'a #:b 'b)
(foo 'a 'b 'c)
> (match (foo 'a 'b 'c)
    [(make/kw foo #:a a #:b b #:c c)
     (list a b c)])
'(a b c)
```

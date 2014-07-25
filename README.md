kw-make-struct
==============

like make from unstable/struct except allowing keywords

[![Build Status](https://travis-ci.org/AlexKnauth/kw-make-struct.png?branch=master)](https://travis-ci.org/AlexKnauth/kw-make-struct)

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
```

#lang racket/base

(provide make/kw make/kw/derived)

(require (except-in unstable/struct make)
         racket/match
         racket/require
         (for-syntax (subtract-in racket/base generic-bind/as-rkt-names)
                     generic-bind/as-rkt-names
                     racket/list
                     racket/local
                     syntax/parse
                     syntax/stx
                     ))

(module+ test
  (require rackunit
           (for-syntax racket/struct-info)))

(begin-for-syntax
  (define (parse-make/kw stx #:orig-form [orig-form stx])
    (syntax-parse stx
      [(make/kw S:id
                (~or pos-arg:expr
                     (~seq kw:keyword kw-arg:expr))
                ...)
       #:fail-when (check-duplicate-keyword
                    (syntax->list #'(kw ...)))
       "duplicate field keyword"
       #:with [fld ...] (stx-map keyword->identifier #'[kw ...])
       (parse-make/fld
        #'(make/fld S [pos-arg] ... [fld kw-arg] ...)
        #:orig-form orig-form)]))

  (define (parse-make/fld stx #:orig-form [orig-form stx])
    (syntax-parse stx
      [(make/fld S:id
                 (~or [pos-arg:expr]
                      [fld:id fld-arg:expr])
                 ...)
       #:fail-when (check-duplicate-identifier
                    (syntax->list #'(fld ...)))
       "duplicate field id"
       (let ()
         (define info (get-struct-info #'S orig-form))
         (define constructor (list-ref info 1))
         (define accessors (list-ref info 3))
         (unless (identifier? #'constructor)
           (raise-syntax-error #f "constructor not available for struct" orig-form #'S))
         (unless (andmap identifier? accessors)
           (raise-syntax-error #f "incomplete info for struct type" orig-form #'S))
         
         (define names
           (local [(define (get-bkwds-struct-names struct-id bkwds-names)
                     (define info (get-struct-info struct-id orig-form))
                     (define name (syntax-e struct-id))
                     (define super (list-ref info 5))
                     (cond [(equal? super #t) (cons name bkwds-names)]
                           [(identifier? super) (get-bkwds-struct-names super (cons name bkwds-names))]
                           [else (display-syntax-warning
                                  #f "warning: incomplete information for struct type"
                                  orig-form struct-id (list #'S))
                                 (cons name bkwds-names)]))]
             (reverse (get-bkwds-struct-names #'S '()))))
         
         (define (accessor->field accessor)
           (local [(define accessor-str (id->sym->str accessor))
                   (define field-str
                     (for/or ([name (in-list names)])
                       (define name-str (symbol->string name))
                       (define name.length (string-length name-str))
                       (cond [(equal? (string-append name-str "-")
                                      (substring accessor-str 0 (add1 name.length)))
                              (substring accessor-str (add1 name.length))]
                             [else #f])))]
             (cond [field-str (string->symbol field-str)]
                   [else (raise-syntax-error #f
                                             (string-append
                                              "cannot infer field name because "
                                              "accessor name doesn't match <struct-name>-<field>")
                                             orig-form accessor)])))
         
         (define fields
           (map accessor->field (reverse accessors)))
         
         (define pos-args
           (syntax->list #'(pos-arg ...)))
         (define fld-args
           (for/hash ([($stx (fld . fld-arg))  (in-list (syntax->list #'([fld . fld-arg] ...)))])
             (define field (syntax-e #'fld))
             (unless (member field fields)
               (raise-syntax-error #f
                                   (format "unexpected field ~a\n  expected fields: ~a"
                                           (syntax-e #'fld)
                                           fields)
                                   orig-form #'fld))
             (values field (syntax-property #'fld-arg 'field field))))

         (let ([num-slots (length accessors)]
               [num-provided (length (syntax->list #'(pos-arg ... fld-arg ...)))])
           (unless (= num-provided num-slots)
             (raise-syntax-error
              #f
              (format (string-append
                       "wrong number of arguments for struct ~s (expected ~s, got ~s)\n"
                       "  expected fields: ~a")
                      (syntax-e #'S)
                      num-slots
                      num-provided
                      fields)
              orig-form)))
         
         (define-values (bkwds-exprs _ __)
           (local [(define (vals #:bkwds-exprs bkwds-exprs #:pos-args pos-args #:fld-args fld-args)
                     (values bkwds-exprs pos-args fld-args))]
             (for/fold ([bkwds-exprs '()] [pos-args pos-args] [fld-args fld-args])
               ([field (in-list fields)])
               (define maybe-expr (hash-ref fld-args field #f))
               (cond [maybe-expr (vals #:bkwds-exprs (cons (syntax-property maybe-expr 'field field)
                                                           bkwds-exprs)
                                       #:pos-args pos-args
                                       #:fld-args (hash-remove fld-args field))]
                     [(empty? pos-args) (raise-syntax-error
                                         #f
                                         (format "missing an argument for the field: ~a" field)
                                         orig-form)]
                     [else (vals #:bkwds-exprs (cons (syntax-property (first pos-args) 'field field)
                                                     bkwds-exprs)
                                 #:pos-args (rest pos-args)
                                 #:fld-args fld-args)]))))
         
         (with-syntax ([constructor constructor]
                       [(expr ...) (reverse bkwds-exprs)])
           (syntax-property #'(constructor expr ...)
                            'disappeared-use
                            (list (syntax-local-introduce #'S)))))]
      ))

  (define (parse-make/kw-match-expander stx #:orig-form [orig-form stx])
    (syntax-parse stx
      [(make/kw S:id
                (~or pos-arg:expr
                     (~seq kw:keyword kw-arg:expr))
                ...)
       #:fail-when (check-duplicate-keyword
                    (syntax->list #'(kw ...)))
       "duplicate field keyword"
       #:with [fld ...] (stx-map keyword->identifier #'[kw ...])
       (parse-make/fld-match-expander
        #'(make/fld S [pos-arg] ... [fld kw-arg] ...)
        #:orig-form orig-form)]))

  (define (parse-make/fld-match-expander stx #:orig-form [orig-form stx])
    (syntax-parse stx
      [(make/fld S:id
                 (~or [pos-arg:expr]
                      [fld:id fld-arg:expr])
                 ...)
       #:fail-when (check-duplicate-identifier
                    (syntax->list #'(fld ...)))
       "duplicate field id"
       (let ()
         (define info (get-struct-info #'S orig-form))
         (define constructor (list-ref info 1))
         (define accessors (list-ref info 3))
         (unless (identifier? #'constructor)
           (raise-syntax-error #f "constructor not available for struct" orig-form #'S))
         (unless (andmap identifier? accessors)
           (raise-syntax-error #f "incomplete info for struct type" orig-form #'S))
         
         (define names
           (local [(define (get-bkwds-struct-names struct-id bkwds-names)
                     (define info (get-struct-info struct-id orig-form))
                     (define name (syntax-e struct-id))
                     (define super (list-ref info 5))
                     (cond [(equal? super #t) (cons name bkwds-names)]
                           [(identifier? super) (get-bkwds-struct-names super (cons name bkwds-names))]
                           [else (display-syntax-warning
                                  #f "warning: incomplete information for struct type"
                                  orig-form struct-id (list #'S))
                                 (cons name bkwds-names)]))]
             (reverse (get-bkwds-struct-names #'S '()))))
         
         (define (accessor->field accessor)
           (local [(define accessor-str (id->sym->str accessor))
                   (define field-str
                     (for/or ([name (in-list names)])
                       (define name-str (symbol->string name))
                       (define name.length (string-length name-str))
                       (cond [(equal? (string-append name-str "-")
                                      (substring accessor-str 0 (add1 name.length)))
                              (substring accessor-str (add1 name.length))]
                             [else #f])))]
             (cond [field-str (string->symbol field-str)]
                   [else (raise-syntax-error #f
                                             (string-append
                                              "cannot infer field name because "
                                              "accessor name doesn't match <struct-name>-<field>")
                                             orig-form accessor)])))
         
         (define fields
           (map accessor->field (reverse accessors)))
         
         (define pos-args
           (syntax->list #'(pos-arg ...)))
         (define fld-args
           (for/hash ([($stx (fld . fld-arg))  (in-list (syntax->list #'([fld . fld-arg] ...)))])
             (define field (syntax-e #'fld))
             (unless (member field fields)
               (raise-syntax-error #f
                                   (format "unexpected field ~a\n  expected fields: ~a"
                                           (syntax-e #'fld)
                                           fields)
                                   orig-form #'fld))
             (values field (syntax-property #'fld-arg 'field field))))
         
         (define-values (bkwds-exprs _ __)
           (local [(define (vals #:bkwds-exprs bkwds-exprs #:pos-args pos-args #:fld-args fld-args)
                     (values bkwds-exprs pos-args fld-args))]
             (for/fold ([bkwds-exprs '()] [pos-args pos-args] [fld-args fld-args])
               ([field (in-list fields)])
               (define maybe-expr (hash-ref fld-args field #f))
               (cond [maybe-expr (vals #:bkwds-exprs (cons (syntax-property maybe-expr 'field field)
                                                           bkwds-exprs)
                                       #:pos-args pos-args
                                       #:fld-args (hash-remove fld-args field))]
                     [(empty? pos-args) (vals #:bkwds-exprs (cons (syntax-property #'_ 'field field)
                                                                  bkwds-exprs)
                                              #:pos-args '()
                                              #:fld-args fld-args)]
                     [else (vals #:bkwds-exprs (cons (syntax-property (first pos-args) 'field field)
                                                     bkwds-exprs)
                                 #:pos-args (rest pos-args)
                                 #:fld-args fld-args)]))))
         
         (with-syntax ([(expr ...) (reverse bkwds-exprs)])
           #'(S expr ...)))]
      ))
  )
(begin-for-syntax
  (define (display-syntax-warning . args)
    (with-handlers ([exn:fail:syntax? display-exn])
      (apply raise-syntax-error args)))
  (define (display-exn exn)
    (define display-handler (error-display-handler))
    (display-handler (exn-message exn) exn))
  (define (check-duplicate-keyword kws)
    (maybe-identifier->keyword
     (check-duplicate-identifier
      (map keyword->identifier kws))))
  (define (id->sym->str id)
    (symbol->string (syntax-e id)))
  (define (kw-stx->kw->str kw-stx)
    (keyword->string (syntax-e kw-stx)))
  (define (kw-stx->kw->str->sym kw-stx)
    (string->symbol (kw-stx->kw->str kw-stx)))
  (define (keyword->identifier kw)
    (define sym (kw-stx->kw->str->sym kw))
    (datum->syntax kw sym kw kw))
  (define (maybe-identifier->keyword id)
    (if id
        (let ([kw (string->keyword (id->sym->str id))])
          (datum->syntax id kw id id))
        #f))
  )



(define-match-expander make/fld
  parse-make/fld-match-expander
  parse-make/fld)

(define-match-expander make/fld/derived
  (syntax-parser
    [(make/fld/derived orig-form S:id args ...)
     (parse-make/fld-match-expander #'(make/fld S args ...) #:orig-form #'orig-form)])
  (syntax-parser
    [(make/fld/derived orig-form S:id args ...)
     (parse-make/fld #'(make/fld S args ...) #:orig-form #'orig-form)]))

(define-match-expander make/kw
  parse-make/kw-match-expander
  parse-make/kw)

(define-match-expander make/kw/derived
  (syntax-parser
    [(make/kw/derived orig-form S:id args ...)
     (parse-make/kw-match-expander #'(make/kw S args ...) #:orig-form #'orig-form)])
  (syntax-parser
    [(make/kw/derived orig-form S:id args ...)
     (parse-make/kw #'(make/kw S args ...) #:orig-form #'orig-form)]))




(module+ test
  (test-case "(make/kw foo ...)"
    (struct foo (a b c))
    (for ([x (in-list (list ;; all by-position
                            (make/kw foo 'a 'b 'c)
                            (make/fld foo ['a] ['b] ['c])
                            ;; one kw
                            ;; #:a
                            (make/kw foo #:a 'a 'b 'c)
                            (make/kw foo 'b #:a 'a 'c)
                            (make/kw foo 'b 'c #:a 'a)
                            (make/fld foo [a 'a] ['b] ['c])
                            (make/fld foo ['b] [a 'a] ['c])
                            (make/fld foo ['b] ['c] [a 'a])
                            ;; #:b
                            (make/kw foo #:b 'b 'a 'c)
                            (make/kw foo 'a #:b 'b 'c)
                            (make/kw foo 'a 'c #:b 'b)
                            (make/fld foo [b 'b] ['a] ['c])
                            (make/fld foo ['a] [b 'b] ['c])
                            (make/fld foo ['a] ['c] [b 'b])
                            ;; #:c
                            (make/kw foo #:c 'c 'a 'b)
                            (make/kw foo 'a #:c 'c 'b)
                            (make/kw foo 'a 'b #:c 'c)
                            (make/fld foo [c 'c] ['a] ['b])
                            (make/fld foo ['a] [c 'c] ['b])
                            (make/fld foo ['a] ['b] [c 'c])
                            ;; two kws
                            ;; #:a and #:b
                            (make/kw foo #:a 'a #:b 'b 'c)
                            (make/kw foo #:a 'a 'c #:b 'b)
                            (make/kw foo #:b 'b #:a 'a 'c)
                            (make/kw foo #:b 'b 'c #:a 'a)
                            (make/kw foo 'c #:a 'a #:b 'b)
                            (make/kw foo 'c #:b 'b #:a 'a)
                            (make/fld foo [a 'a] [b 'b] ['c])
                            (make/fld foo [a 'a] ['c] [b 'b])
                            (make/fld foo [b 'b] [a 'a] ['c])
                            (make/fld foo [b 'b] ['c] [a 'a])
                            (make/fld foo ['c] [a 'a] [b 'b])
                            (make/fld foo ['c] [b 'b] [a 'a])
                            ;; #:a and #:c
                            (make/kw foo #:a 'a #:c 'c 'b)
                            (make/kw foo #:a 'a 'b #:c 'c)
                            (make/kw foo #:c 'c #:a 'a 'b)
                            (make/kw foo #:c 'c 'b #:a 'a)
                            (make/kw foo 'b #:a 'a #:c 'c)
                            (make/kw foo 'b #:c 'c #:a 'a)
                            (make/fld foo [a 'a] [c 'c] ['b])
                            (make/fld foo [a 'a] ['b] [c 'c])
                            (make/fld foo [c 'c] [a 'a] ['b])
                            (make/fld foo [c 'c] ['b] [a 'a])
                            (make/fld foo ['b] [a 'a] [c 'c])
                            (make/fld foo ['b] [c 'c] [a 'a])
                            ;; #:b and #:c
                            (make/kw foo #:b 'b #:c 'c 'a)
                            (make/kw foo #:b 'b 'a #:c 'c)
                            (make/kw foo #:c 'c #:b 'b 'a)
                            (make/kw foo #:c 'c 'a #:b 'b)
                            (make/kw foo 'a #:b 'b #:c 'c)
                            (make/kw foo 'a #:c 'c #:b 'b)
                            (make/fld foo [b 'b] [c 'c] ['a])
                            (make/fld foo [b 'b] ['a] [c 'c])
                            (make/fld foo [c 'c] [b 'b] ['a])
                            (make/fld foo [c 'c] ['a] [b 'b])
                            (make/fld foo ['a] [b 'b] [c 'c])
                            (make/fld foo ['a] [c 'c] [b 'b])
                            ;; all kws
                            (make/kw foo #:a 'a #:b 'b #:c 'c)
                            (make/kw foo #:a 'a #:c 'c #:b 'b)
                            (make/kw foo #:b 'b #:a 'a #:c 'c)
                            (make/kw foo #:b 'b #:c 'c #:a 'a)
                            (make/kw foo #:c 'c #:a 'a #:b 'b)
                            (make/kw foo #:c 'c #:b 'b #:a 'a)
                            (make/fld foo [a 'a] [b 'b] [c 'c])
                            (make/fld foo [a 'a] [c 'c] [b 'b])
                            (make/fld foo [b 'b] [a 'a] [c 'c])
                            (make/fld foo [b 'b] [c 'c] [a 'a])
                            (make/fld foo [c 'c] [a 'a] [b 'b])
                            (make/fld foo [c 'c] [b 'b] [a 'a])
                            ))])
      (check-equal? (foo-a x) 'a)
      (check-equal? (foo-b x) 'b)
      (check-equal? (foo-c x) 'c)
      (check-match x (and (make/kw foo 'a 'b 'c)
                          (make/kw foo #:a 'a 'b 'c)
                          (make/kw foo #:a 'a #:b 'b #:c 'c)
                          (make/fld foo ['a] ['b] ['c])
                          (make/fld foo [a 'a] ['b] ['c])
                          (make/fld foo [a 'a] [b 'b] [c 'c])
                          ))
      )
    #;(displayln "done testing (make/kw foo ...)"))
  
  (test-case "(make/kw new-pair ...)"
    (define (new-pair? x) (pair? x))
    (define (new-pair-car x) (car x))
    (define (new-pair-cdr x) (cdr x))
    (define-syntax new-pair
      (make-struct-info
       (λ () (list #f
                   #'cons
                   #'new-pair?
                   (list #'new-pair-cdr #'new-pair-car)
                   (list #f #f)
                   #t))))
    (for ([x (in-list (list ;; all by-position
                            (make/kw new-pair 'car 'cdr)
                            (make/fld new-pair ['car] ['cdr])
                            ;; one kw
                            ;; #:car
                            (make/kw new-pair #:car 'car 'cdr)
                            (make/kw new-pair 'cdr #:car 'car)
                            (make/fld new-pair [car 'car] ['cdr])
                            (make/fld new-pair ['cdr] [car 'car])
                            ;; #:cdr
                            (make/kw new-pair #:cdr 'cdr 'car)
                            (make/kw new-pair 'car #:cdr 'cdr)
                            (make/fld new-pair [cdr 'cdr] ['car])
                            (make/fld new-pair ['car] [cdr 'cdr])
                            ;; all kws
                            (make/kw new-pair #:car 'car #:cdr 'cdr)
                            (make/kw new-pair #:cdr 'cdr #:car 'car)
                            (make/fld new-pair [car 'car] [cdr 'cdr])
                            (make/fld new-pair [cdr 'cdr] [car 'car])
                            ))])
      (check-equal? (car x) 'car)
      (check-equal? (cdr x) 'cdr)
      (check-match x (and ;; all by-position
                          (make/kw new-pair 'car 'cdr)
                          (make/fld new-pair ['car] ['cdr])
                          ;; one kw
                          ;; #:car
                          (make/kw new-pair #:car 'car 'cdr)
                          (make/kw new-pair 'cdr #:car 'car)
                          (make/fld new-pair [car 'car] ['cdr])
                          (make/fld new-pair ['cdr] [car 'car])
                          ;; #:cdr
                          (make/kw new-pair #:cdr 'cdr 'car)
                          (make/kw new-pair 'car #:cdr 'cdr)
                          (make/fld new-pair [cdr 'cdr] ['car])
                          (make/fld new-pair ['car] [cdr 'cdr])
                          ;; all kws
                          (make/kw new-pair #:car 'car #:cdr 'cdr)
                          (make/kw new-pair #:cdr 'cdr #:car 'car)
                          (make/fld new-pair [car 'car] [cdr 'cdr])
                          (make/fld new-pair [cdr 'cdr] [car 'car])
                          ;; with blanks
                          (make/kw new-pair)
                          (make/kw new-pair 'car)
                          (make/kw new-pair #:car 'car)
                          (make/kw new-pair #:cdr 'cdr)
                          (make/fld new-pair)
                          (make/fld new-pair ['car])
                          (make/fld new-pair [car 'car])
                          (make/fld new-pair [cdr 'cdr])
                          ))
      )
    #;(displayln "done testing (make/kw new-pair ...)")
    ))

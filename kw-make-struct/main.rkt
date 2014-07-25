#lang racket/base
(require (except-in unstable/struct make)
         (for-syntax racket/base
                     racket/list
                     racket/local
                     rackjure/threading
                     syntax/parse))

(module+ test
  (require rackunit
           (for-syntax racket/struct-info)))

(define-syntax (make/kw stx)
  (syntax-parse stx
    [(make/kw S:id
              (~or pos-arg:expr
                   (~seq kw:keyword kw-arg:expr))
              ...)
     (let ()
       (define info (get-struct-info #'S stx))
       (define constructor (list-ref info 1))
       (define accessors (list-ref info 3))
       (unless (identifier? #'constructor)
         (raise-syntax-error #f "constructor not available for struct" stx #'S))
       (unless (andmap identifier? accessors)
         (raise-syntax-error #f "incomplete info for struct type" stx #'S))
       (let ([num-slots (length accessors)]
             [num-provided (length (syntax->list #'(pos-arg ... kw-arg ...)))])
         (unless (= num-provided num-slots)
           (raise-syntax-error
            #f
            (format "wrong number of arguments for struct ~s (expected ~s, got ~s)"
                    (syntax-e #'S)
                    num-slots
                    num-provided)
            stx)))
       
       (define names
         (local [(define (get-bkwds-struct-names struct-id bkwds-names)
                   (define info (get-struct-info struct-id stx))
                   (define name (syntax-e struct-id))
                   (define super (list-ref info 5))
                   (cond [(equal? super #t) (cons name bkwds-names)]
                         [(identifier? super) (get-bkwds-struct-names super (cons name bkwds-names))]
                         [else (display-syntax-warning
                                #f "warning: incomplete information for struct type"
                                stx struct-id (list #'S))
                               (cons name bkwds-names)]))]
           (reverse (get-bkwds-struct-names #'S '()))))
       
       (define (accessor->field accessor)
         (local [(define accessor-str (~> accessor syntax-e symbol->string))
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
                                           stx accessor)])))
       
       (define fields
         (map accessor->field (reverse accessors)))
       
       (define pos-args
         (syntax->list #'(pos-arg ...)))
       (define kw-args
         (make-immutable-hash
          (for/list ([term (in-list (syntax->list #'([kw . kw-arg] ...)))])
            (with-syntax ([(kw . kw-arg) term])
              (define field (~> #'kw syntax-e keyword->string string->symbol))
              (unless (member field fields)
                (raise-syntax-error #f "unexpected field keyword" stx #'kw))
              (cons field (syntax-property #'kw-arg 'field field))))))
       
       (define-values (bkwds-exprs _ __)
         (local [(define (vals #:bkwds-exprs bkwds-exprs #:pos-args pos-args #:kw-args kw-args)
                   (values bkwds-exprs pos-args kw-args))]
           (for/fold ([bkwds-exprs '()] [pos-args pos-args] [kw-args kw-args])
             ([field (in-list fields)])
             (define maybe-expr (hash-ref kw-args field #f))
             (cond [maybe-expr (vals #:bkwds-exprs (cons (syntax-property maybe-expr 'field field)
                                                         bkwds-exprs)
                                     #:pos-args pos-args
                                     #:kw-args (hash-remove kw-args field))]
                   [(empty? pos-args) (error 'make/kw "missing an argument for the field: ~a" field)]
                   [else (vals #:bkwds-exprs (cons (syntax-property (first pos-args) 'field field)
                                                   bkwds-exprs)
                               #:pos-args (rest pos-args)
                               #:kw-args kw-args)]))))
       
       (with-syntax ([constructor constructor]
                     [(expr ...) (reverse bkwds-exprs)])
         (syntax-property #'(constructor expr ...)
                          'disappeared-use
                          #'S)))]
    ))

(begin-for-syntax
  (define (display-syntax-warning . args)
    (with-handlers ([exn:fail:syntax? display-exn])
      (apply raise-syntax-error args)))
  (define (display-exn exn)
    (define display-handler (error-display-handler))
    (display-handler (exn-message exn) exn))
  )


(module+ test
  (test-case "(make/kw foo ...)"
    (struct foo (a b c))
    (for ([x (in-list (list ;; all by-position
                            (make/kw foo 'a 'b 'c)
                            ;; one kw
                            ;; #:a
                            (make/kw foo #:a 'a 'b 'c)
                            (make/kw foo 'b #:a 'a 'c)
                            (make/kw foo 'b 'c #:a 'a)
                            ;; #:b
                            (make/kw foo #:b 'b 'a 'c)
                            (make/kw foo 'a #:b 'b 'c)
                            (make/kw foo 'a 'c #:b 'b)
                            ;; #:c
                            (make/kw foo #:c 'c 'a 'b)
                            (make/kw foo 'a #:c 'c 'b)
                            (make/kw foo 'a 'b #:c 'c)
                            ;; two kws
                            ;; #:a and #:b
                            (make/kw foo #:a 'a #:b 'b 'c)
                            (make/kw foo #:a 'a 'c #:b 'b)
                            (make/kw foo #:b 'b #:a 'a 'c)
                            (make/kw foo #:b 'b 'c #:a 'a)
                            (make/kw foo 'c #:a 'a #:b 'b)
                            (make/kw foo 'c #:b 'b #:a 'a)
                            ;; #:a and #:c
                            (make/kw foo #:a 'a #:c 'c 'b)
                            (make/kw foo #:a 'a 'b #:c 'c)
                            (make/kw foo #:c 'c #:a 'a 'b)
                            (make/kw foo #:c 'c 'b #:a 'a)
                            (make/kw foo 'b #:a 'a #:c 'c)
                            (make/kw foo 'b #:c 'c #:a 'a)
                            ;; #:b and #:c
                            (make/kw foo #:b 'b #:c 'c 'a)
                            (make/kw foo #:b 'b 'a #:c 'c)
                            (make/kw foo #:c 'c #:b 'b 'a)
                            (make/kw foo #:c 'c 'a #:b 'b)
                            (make/kw foo 'a #:b 'b #:c 'c)
                            (make/kw foo 'a #:c 'c #:b 'b)
                            ;; all kws
                            (make/kw foo #:a 'a #:b 'b #:c 'c)
                            (make/kw foo #:a 'a #:c 'c #:b 'b)
                            (make/kw foo #:b 'b #:a 'a #:c 'c)
                            (make/kw foo #:b 'b #:c 'c #:a 'a)
                            (make/kw foo #:c 'c #:a 'a #:b 'b)
                            (make/kw foo #:c 'c #:b 'b #:a 'a)
                            ))])
      (check-equal? (foo-a x) 'a)
      (check-equal? (foo-b x) 'b)
      (check-equal? (foo-c x) 'c))
    #;(displayln "done testing (make/kw foo ...)"))
  
  (test-case "(make/kw new-pair ...)"
    (define (new-pair? x) (displayln "new pair?") (pair? x))
    (define (new-pair-car x) (displayln "new car") (car x))
    (define (new-pair-cdr x) (displayln "new cdr") (cdr x))
    (define-syntax new-pair
      (make-struct-info
       (λ () (list #f
                   #'cons
                   #'new-pair?
                   (list #'new-pair-cdr #'new-pair-car)
                   (list #f #f)
                   #t))))
    (for ([x (in-list (list (make/kw new-pair 'car 'cdr)
                            (make/kw new-pair #:car 'car 'cdr)
                            (make/kw new-pair 'cdr #:car 'car)
                            (make/kw new-pair #:cdr 'cdr 'car)
                            (make/kw new-pair 'car #:cdr 'cdr)
                            (make/kw new-pair #:car 'car #:cdr 'cdr)
                            (make/kw new-pair #:cdr 'cdr #:car 'car)
                            ))])
      (check-equal? (car x) 'car)
      (check-equal? (cdr x) 'cdr))
    #;(displayln "done testing (make/kw new-pair ...)")
    ))
#lang rosette
(require syntax/parse)
(require (for-syntax syntax/parse))
(require (prefix-in blk: racket/block))
(require racket/stxparam)
(require (for-syntax racket/stxparam))
(provide 
 (rename-out [symb-def define-symbolic])
 (rename-out [get-path get-constraints])
 #%module-begin
 <
 >
 <=
 >=
 +
 -
 /
 #%top-interaction
 #%app
 #%datum
 quote
 get-inputs)

;; Defines the parameter tar that is used to escape get-path
(define-syntax-parameter tar #'#f)
;; Creates a continuation-prompt-tag cont-prompt that can be used to escape
;; get-path
(define cont-prompt
  (make-continuation-prompt-tag))

;; Syntax -> Syntax
;; Gets the restrictions on the symbolic values that can get you from the funtion
;; you're currently in to the function you specify
(define-syntax get-path
  (syntax-parser
    [(_ name app)
     #'(syntax-parameterize
           [(tar #''name)]
         (call-with-continuation-prompt (λ () app #f) cont-prompt (λ (x) x)))]))

;; Syntax -> Syntax
;; Gets a set of inputs that will allow you to reach the desired function, if possible
;; If it's not possible, return (unsat)
(define-syntax get-inputs
  (syntax-parser
    [(_ name app)
     #'(solve(assert(get-path name app)))]))

;; Syntax -> Syntax
;; Creates a symbolic function, lets concrete values be, but makes any
;; free identifiers into symbolic values
(define-syntax (symb-def stx)
  (syntax-parse stx
    [(_ (name arg:id ...) expr)
     #:with n-tag (parse-bsl #'expr #'name)
     #'(define-syntax (name stx)
         (syntax-parse stx
           [(name app-arg (... ...))
            #:with (inner-arg-id (... ...)) #'(arg ...)
            #:fail-unless (equal? (length (attribute inner-arg-id))
                                  (length (attribute  app-arg)))
            (format "Arity mismatch expected: ~a received: ~a"
                    (length (attribute inner-arg-id))
                    (length (attribute  app-arg)))
            #:with ((det-args (... ...))
                    (det-names (... ...))
                    (undet-args (... ...))
                    (undet-names (... ...)))
            (split-args #'(inner-arg-id (... ...)) #'(app-arg (... ...)))
            #:with b (syntax-parameter-value #'tar)
            #`(blk:block
               (define det-names det-args) (... ...)
               (define undet-names
                 (local ((define-symbolic undet-args integer?)) undet-args))
               (... ...)
               (if (and (symbol? b)(symbol=? 'name b))
                   (abort-current-continuation cont-prompt (pc))
                   n-tag ))]))]))

;; An argList is:
;; [Listof [Listof Syntax] [Listof Syntax] [Listof Syntax] [Listof Syntax]]
;; Interpretation: The first list is the list of determined arguments.
;; The second list is the list of names for the determined arguments.
;; The third list is the list of undetermined arguments
;; The fourth list is the list of the names for those undetermined arguments.


;; Syntax Syntax -> argList
;; Filters across the inargs (the names) and blargs (the values) and creates an
;; argsList pairing the numeric arguments with numeric names
;; and the non numeric arguments with the non numeric names
(define-for-syntax (split-args inargs blargs)
  (define loi0 (syntax->list inargs))
  (define loa0 (syntax->list blargs))
  (define (split-list loi loa)
    (cond
      [(empty? loi) (list '() '() '() '())]
      [else (local
              ((define res (split-list (rest loi) (rest loa)))
               (define determined (first res))
               (define determined-name (second res))
               (define undet (third res))
               (define undet-name (fourth res)))
              (cond
                [(list? (syntax->datum (first loa)))
                 (list (cons (first loa) determined)
                       (cons (first loi) determined-name)
                       undet
                       undet-name)]
                [(integer? (syntax->datum (first loa)))
                 (list (cons (first loa) determined)
                       (cons (first loi) determined-name)
                       undet
                       undet-name)]
                [(identifier-binding (first loa))
                 (list (cons (first loa) determined)
                       (cons (first loi) determined-name)
                       undet
                       undet-name)]
                [else
                 (list determined
                       determined-name
                       (cons (first loa) undet)
                       (cons (first loi) undet-name))]))]))
  (define ret (split-list loi0 loa0))
  ret)

;; Syntax -> Syntax
;; parses things inside symbolic definitions with BSL-
(define-for-syntax (parse-bsl expr cs)
  (syntax-parse expr
    ;if0, does "then" if the first argument is a 0, and else if not
    [((~datum if0) q then else)
     #:with q+ (parse-bsl #'q cs)
     #:with then+ (parse-bsl #'then cs)
     #:with else+ (parse-bsl #'else cs)
     #'(if (zero? q+) then+ else+)]
    ;or0 returns 0 if at least one of farg and sarg are 0, else
    ;return -1
    [((~datum or0) farg sarg)
     #:with farg+ (parse-bsl #'farg cs)
     #:with sarg+ (parse-bsl #'sarg cs)
     #'(if (or (zero? farg) (zero? sarg)) 0 -1)]
    ;and0 returns 0 if both farg and sarg are 0, else return -1
    [((~datum and0) farg sarg)
     #:with farg+ (parse-bsl #'farg cs)
     #:with sarg+ (parse-bsl #'sarg cs)
     #'(if (and (zero? farg+) (zero? sarg+) ) 0 -1)]
    ;returns 0 if farg is less than or equal to sarg, else return -1
    [((~datum <=) farg sarg)
     #:with farg+ (parse-bsl #'farg cs)
     #:with sarg+ (parse-bsl #'sarg cs)
     #'(if (<= farg+ sarg+) 0 -1)]
    ;returns 0 if farg is greater than or equal to sarg, else return -1
    [((~datum >=) farg sarg)
     #:with farg+ (parse-bsl #'farg cs)
     #:with sarg+ (parse-bsl #'sarg cs)
     #'(if (>= farg+ sarg+) 0 -1)]
    ;returns 0 if farg is less than sarg, else return -1
    [((~datum <) farg sarg)
     #:with farg+ (parse-bsl #'farg cs)
     #:with sarg+ (parse-bsl #'sarg cs)
     #'(if (< farg+ sarg+) 0 -1)]
    ;returns 0 if farg is greater than sarg, else return -1
    [((~datum >) farg sarg)
     #:with farg+ (parse-bsl #'farg cs)
     #:with sarg+ (parse-bsl #'sarg cs)
     #'(if (> farg+ sarg+) 0 -1)]
    ;adds farg and sarg
    [((~datum +) farg sarg)
     #:with farg+ (parse-bsl #'farg cs)
     #:with sarg+ (parse-bsl #'sarg cs)
     #'(+ farg+ sarg+)]
    ;substracts farg and sarg
    [((~datum -) farg sarg)
     #:with farg+ (parse-bsl #'farg cs)
     #:with sarg+ (parse-bsl #'sarg cs)
     #'(- farg+ sarg+)]
    ;divides farg and sarg
    [((~datum /) farg sarg)
     #:with farg+ (parse-bsl #'farg cs)
     #:with sarg+ (parse-bsl #'sarg cs)
     #'(/ farg+ sarg+)]
    ;multiplies farg and sarg
    [((~datum *) farg sarg)
     #:with farg+ (parse-bsl #'farg cs)
     #:with sarg+ (parse-bsl #'sarg cs)
     #'(* farg+ sarg+)]
    ;adds 1 to farg
    [((~datum add1) farg)
     #:with farg+ (parse-bsl #'farg cs)
     #'(add1 farg+)]
    ;subs 1 from farg
    [((~datum sub1) farg)
     #:with farg+ (parse-bsl #'farg cs)
     #'(add1 farg+)] 
    ;determine if something parsed is a number
    [a:number #'a]
    [a:id #'a]
    [(nn:id args ...)
     #:with fname cs
     #:fail-unless (comp #'nn #'fname) "Recursion is currently not supported in SBL"
     #:fail-when (and (not (identifier-binding #'nn)) #'nn) "Not bound"
     #'(nn args ...)]))

;; Syntax -> Boolean
;; returns if both of the syntaxes passed in are intenionally equal.
(define-for-syntax (comp new-func old-func)
  (not (free-identifier=?  new-func old-func)))








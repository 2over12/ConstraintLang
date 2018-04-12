#lang rosette/safe
(require syntax/parse)
(require (for-syntax syntax/parse))
(require racket/syntax)
(require (for-syntax (prefix-in rs: rosette)))
(require (for-syntax (prefix-in rkt: racket/base)))
(require (prefix-in blk: racket/block))

(provide 
 (rename-out [symb-def define-symbolic])
 (rename-out [quick-def def])
 <
 >
 <=
 >=
 +
 -
 /
 *
 #%module-begin
 #%top-interaction
 #%app
 #%datum
 quote
 )


#;(define-syntax (symb-def stx)
    (syntax-parse stx
      [(_ (name arg:id ...) expr)
       #:with n-tag (parse-bsl #'expr #'name)
       #'(define (name arg ...) (local ((define saved arg))
                                  (set! arg (local ((define-symbolic arg integer?))
                                              (if (number? saved) saved arg)))) ... n-tag)]))

#;(define-syntax (symb-def stx)
    (syntax-parse stx
      [(_ (name arg:id ...) expr)
       #:with n-tag (parse-bsl #'expr #'name)
       #'(define-syntax (name stx)
           (syntax-parse stx
             [(_ app-arg (... ...))
              #:with (inner-arg-id (... ...)) #'(arg ...)
              #`(begin (define inner-arg-id app-arg) (... ...)
                       (local ((define saved inner-arg-id))
                         (set! inner-arg-id (local ((define-symbolic #,app-arg integer?))
                                              (if (number? saved) saved inner-arg-id)))) (... ...)
                                                                                         n-tag)]))]))

(define-syntax (symb-def stx)
  (syntax-parse stx
    [(_ (name arg:id ...) expr)
     #:with n-tag (parse-bsl #'expr #'name)
     #'(define-syntax (name stx)
         (syntax-parse stx
           [(name app-arg (... ...))
            #:with (inner-arg-id (... ...)) #'(arg ...)
            #:with ((det-args (... ...))
                    (det-names (... ...))
                    (undet-args (... ...))
                    (undet-names (... ...)))
            (split-args #'(inner-arg-id (... ...)) #'(app-arg (... ...)))
            #`(blk:block
                (define det-names det-args) (... ...)
                (define undet-names (local ((define-symbolic undet-args integer?)) undet-args))
                (... ...)
                n-tag)]))]))
;; An argList is:
;; [Listof [Listof Syntax] [Listof Syntax] [Listof Syntax] [Listof Syntax]]
;; Interpretation: The first list is the list of bound arguments.
;; The second list is the list of names for the bound arguments.
;; The third list is the list of unbound arguments
;; The fourth list is the list of the names for those unbound arguments.


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

#;(define-for-syntax (make-define def-args^ body^)
    (define/syntax-parse (def-args ...) def-args^)
    (define/syntax-parse body body^)
    (syntax-parser
      [(_ app-args ...) #'(let ([def-args app-args] ...) body)]))

#;(define-syntax (symb-def stx)
    (syntax-parser
      [(_ (name arg:id ...) expr)
       #:with n-tag (parse-bsl #'expr #'name)
       #'(define-syntax name
           (make-define #'(arg ...) #'n-tag))]))

(define-for-syntax (make-qdef def-args^ body^)
  (define/syntax-parse (def-args ...) def-args^)
  (define/syntax-parse body body^)
  (syntax-parser
    [(_ app-args ...) #'(let ([def-args app-args] ...) body)]))

(define-syntax (quick-def stx)
  (syntax-parser
    [(_ (name args ...) body)
     #'(define-syntax name
         (make-qdef #'(args ...) #'body))]))



#;(if (number? arg)
      (void)
      (set! arg
            (local
              (define-symbolic app-arg interger?))))



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
    ;determine if something parsed is a number
    [a:number #'a]
    [a:id #'a]
    [(nn:id args ...)
     #:with fname cs
     #:fail-unless (comp #'nn #'fname) "Recursion is currently not supported in SBL"
     #'(nn args ...)]))


(define-for-syntax (comp newFunc oldFunc)
  (not (eq? (syntax->datum newFunc)
            (syntax->datum oldFunc))))








#lang rosette/safe
;(require racket)
(require syntax/parse)
(require (for-syntax syntax/parse))

(provide 
 (rename-out [symb-def define-symbolic])
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
     ;; #:with (+arg +arg2 ...) (expand-args #'(arg arg2 ...))
     #:with n-tag (parse-bsl #'expr #'name)
     #'(define (name arg ...) (local ((define saved arg))
                                (set! arg (local ((define-symbolic arg integer?))
            (if (number? saved) saved arg)))) ... n-tag)]))

(define-syntax (symb-def stx)
  (syntax-parse stx
    [(_ (name arg:id ...) expr)
    #:with n-tag (parse-bsl #'expr #'name)
    #'(define-syntax (name stx)
        (syntax-parse stx
            [(_ app-arg (... ...))
             #`(begin (define arg app-arg) ...  (... ...) (displayln arg) ...)]))]))



(define-for-syntax (parse-bsl expr func)
  (syntax-parse expr
    ;if0, does "then" if the first argument is a 0, and else if not
    [((~datum if0) q then else)
     #:with q+ (parse-bsl #'q)
     #:with then+ (parse-bsl #'then func)
     #:with else+ (parse-bsl #'else func)
     #'(if (zero? q+) then+ else+)]
    ;or0 returns 0 if at least one of farg and sarg are 0, else
    ;return -1
    [((~datum or0) farg sarg)
     #:with farg+ (parse-bsl #'farg func)
     #:with sarg+ (parse-bsl #'sarg func)
     #'(if (or (zero? farg) (zero? sarg)) 0 -1)]
    ;and0 returns 0 if both farg and sarg are 0, else return -1
    [((~datum and0) farg sarg)
     #:with farg+ (parse-bsl #'farg func)
     #:with sarg+ (parse-bsl #'sarg func)
     #'(if (and (zero? farg+) (zero? sarg+) ) 0 -1)]
    ;returns 0 if farg is less than or equal to sarg, else return -1
    [((~datum <=) farg sarg)
     #:with farg+ (parse-bsl #'farg func)
     #:with sarg+ (parse-bsl #'sarg func)
     #'(if (<= farg+ sarg+) 0 -1)]
    ;returns 0 if farg is greater than or equal to sarg, else return -1
    [((~datum >=) farg sarg)
     #:with farg+ (parse-bsl #'farg func)
     #:with sarg+ (parse-bsl #'sarg func)
     #'(if (>= farg+ sarg+) 0 -1)]
    ;returns 0 if farg is less than sarg, else return -1
    [((~datum <) farg sarg)
     #:with farg+ (parse-bsl #'farg func)
     #:with sarg+ (parse-bsl #'sarg func)
     #'(if (< farg+ sarg+) 0 -1)]
    ;returns 0 if farg is greater than sarg, else return -1
    [((~datum >) farg sarg)
     #:with farg+ (parse-bsl #'farg func)
     #:with sarg+ (parse-bsl #'sarg func)
     #'(if (> farg+ sarg+) 0 -1)]
    ;adds farg and sarg
    [((~datum +) farg sarg)
     #:with farg+ (parse-bsl #'farg func)
     #:with sarg+ (parse-bsl #'sarg func)
     #'(+ farg+ sarg+)]
    ;substracts farg and sarg
    [((~datum -) farg sarg)
     #:with farg+ (parse-bsl #'farg func)
     #:with sarg+ (parse-bsl #'sarg func)
     #'(- farg+ sarg+)]
    ;divides farg and sarg
    [((~datum /) farg sarg)
     #:with farg+ (parse-bsl #'farg func)
     #:with sarg+ (parse-bsl #'sarg func)
     #'(/ farg+ sarg+)]
    ;multiplies farg and sarg
    [((~datum *) farg sarg)
     #:with farg+ (parse-bsl #'farg func)
     #:with sarg+ (parse-bsl #'sarg func)
     #'(* farg+ sarg+)]
    ;determine if something parsed is a number
    [a:number #'a]
    [a:id #'a]
    [(#%app name:id args ...)
     #:with (args+ ...) #`((#,parse-bsl #'args name) ...)
     #'(#%app name args+ ...)]))







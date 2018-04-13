#lang s-exp "bslLow.rkt"
;; Define some symbolic functions
(define-symbolic (g a)  42)
(define-symbolic (t b) (if0 (> b 4) 4 2))
(define-symbolic (z a) (+ a 7))
(define-symbolic (x c d) (if0 (>= (z d) 4 ) (t c) (g 1)))
(define-symbolic (f a b) (if0 (< (sub1 a) 10) (x a b) 4))
;(define-symbolic (j m) (j m))

;; Examples of function calls
;(f i e)
;(f 1 2)
;(f i 2)
#;(f a)

;; There are no restrictions on the constraints, so returns #t
(get-constraints g (g b))
;; All inputs satisfy this case, so 
(get-inputs g (g b))

;; Actual
(get-constraints g (f i e))
(get-inputs g (f i e))

;; You can't get to g from f, therefore #f
(get-constraints g (f 100 e))
;; Since you can't get from f to g, you also don't have any inputs that
;; will satisfy 
(get-inputs g (f 100 e))

;; Interesting
#;(define-symbolic (h n) (f (x n 4) b))
#;(get-inputs g (h a))



;; Mutual Recursion?
#;(define-symbolic (this f2 f1) (that f2 f1))
#;(define-symbolic (that f2 f1) (this f2 f1))
#lang s-exp "bslLow.rkt"

(define-symbolic (x c d) (if0 (<= c d) 12 3))
(define-symbolic (func a ) (x (x 3 4) a))
(func (x f 2))
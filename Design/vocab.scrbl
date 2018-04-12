#lang scribble/sigplan

@(require "utility.rkt")
@title{"Vocab"}
@itemlist[@(vocab "vairable" "a name that is bound")]

@(define (vocab name def) item{@italic{@(string-append name ": ")} def})
#lang scribble/sigplan
 
@title{SBL: Symbolic Basic Language}
 
@abstract{SBL is a language that extends racket, providing the ability to
define functions that can take both concrete and symbolic values. The programmer
can interact with these functions either by using the outputs that SBL provides
or by implementing callback function.}
@include-section["design.scrbl"]
@;@include-section["grammar.scrbl"]
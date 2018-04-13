#lang scribble/manual
@(require (for-label racket))

@defmodule[SBL #:lang]
 
@;Docs Info
@title{@secref{SBL}: Symbolic Basic Language}
@author["Ian Smith and Eddy Li"]
@history[#:added "1.0"
         #:changed "1.1" "Added examples"]

@section{Introduction}
@secref{SBL} (pronounced Sibyl) is a language that extends Racket, providing the ability to define
functions that can take both concrete and symbolic values on the fly.
The programmer can interact with these functions by using the outputs that @secref{SBL}
provides or by interacting with out primitive functions. 

@section[#:tag "SBL"]{@bold{SBL}}

@;;-------------------------------------------------------------------------------

@subsection{@secref{SBL} Vocabulary}
@(racketgrammar*
  [Vocab
   define-symbolic
   var
   get-input
   symb-expr
   bsl-expr])

@itemlist[@item{A @italic{define-symbolic} is for defining symbolic functions.}
          @item{A @italic{var} is a name that can be bound.}
          @item{A @italic{get-input} is a primitive function that gets the requisite inputs to reach
           a given function from the given application.}
          @item{A @italic{symb-expr} is an expression that evaluates to either concrete number
           or a symbolic value.}
          @item{A @italic{bsl-expr} is an expression written in in @secref{BSL}. For more information,
           see @secref{BSL}.}]

@;;------------------------------------------------------------------------

@subsection{@secref{SBL} Grammar}
@(racketgrammar*
  [Functions
   (define-symbolic (variable ...) bsl–expr)
   (get-constraint variable symb-expr)
   (get-inputs variable symb-expr)
   (variable (symb-expr) ...)])

All of the following syntactic forms assume the following functions have been defined in their
examples
@racketblock[
 (define-symbolic (g a) 42)
 (define-symbolic (t b) (if0 (> b 4) 4 2))
 (define-symbolic (c a) (+ a 7))
 (define-symbolic (x c d) (if0 (>= (c d) 4) (t c) (g 1)))
 (define-symbolic (f a b) (if0 (< (sub1 a) 10) (x a b) 4))]

@defform[(define-symbolic (id var ...) bsl-expr)]
Defines a function that takes in the vars you pass in and executes the BSL-expr, replacing any mention
of the vars in the bsl-expr with the values you passed in.

@defform[(get-constraint var symb-expr)]
Produces the constraints on which you can reach a function through the symb-expr passed in. If you
can't reach the var from the symb-expr passed in, then you get #f.

For Example:
@(nested #:style 'code-inset @racketresultblock0[
 > (get-constraints g (g b))
 #t

 > (get-constraints (f 100 e))
 #f

 > (get-constraints (f i e))
 (&& (< (+ 1 i) 10) (! (<= 4 (+ 7 e))))])

@defform[(get-inputs var symb-expr)]
Produces a set of values that can reach the given symbolic application. If any value will do, then
return (model) by itself

For Example:
@(nested #:style 'code-inset @racketresultblock0[
 > (get-inputs g (g b))
 (model)

 > (get-inputs g (f 100 e))
 (unsat)

 > (get-inputs g (f i e))
 (model
  [i 8]
  [e -4])])

@defform[(variable symb-expr ...)]
Applies the function to the given expressions if the variable is an application of a symbolic
function. All expressions must evaluate to either a symbol or number. The produced result is
the application of the function on the constraints, fully expanded.

For Example:

@(nested #:style 'code-inset @racketresultblock0[
 > (f i e)

 (ite (< (+ 1 i) 10) (ite (<= 4 (+ 7 e)) (ite (< 4 i) 4 2) 42) 4)

 >(f 1 2)

 2

 >(f i 2)
 
 (ite (< (+ 1 i) 10) (ite (< 4 i) 4 2) 4)])

@;;--------------------------------------------------------------------------

@subsection{@secref{SBL} Scope}
@itemlist[@item{@italic{(define-symbolic variable expr)} binds variable for all below top level forms
           that are symbolic.}
          @item{@italic{(define-symbolic (variable variable ...) bsl-expr)} binds the first
           variable in all top level forms and the rest of the variables in bsl-expr.}]

@bold{A note on recursion}: In the current build of SBL, recursion of any form is not allowed.
Functions can only be called below where they are defined, and cannot refer to themselves.



@;;------------------------------------------------------------------------------------------------
@;;------------------------------------------------------------------------------------------------


@section[#:tag "BSL"]{@bold{BSL-}}
BSL- is a version of the Beginner Student Language that only support numbers and symbolic expressions,
and does not support recursion.

@subsection{@secref{BSL} Vocabulary}
@(racketgrammar*
  [bsl-expr
   if0
   or0
   and0
   <
   >
   <=
   >=
   +
   -
   /
   *
   add1
   sub1
   number
   symbol])

@defform[(if0 expr1 expr2 expr3)
         #:contracts
         ([expr1 BSL-Expr]
          [expr2 BSL-Expr]
          [expr3 BSL-Expr])]
If the @italic{expr1} is 0, then return @italic{expr2}, otherwise return @italic{expr3}.

Examples:
@(nested #:style 'code-inset @racketresultblock0[
 > (if 0 4 5)

 4

 > (if -1 4 5)

 5

 > (if 10 4 5)

 5])

@defform[(or0 expr1 expr2)
         #:contracts
         ([expr1 BSL-Expr]
          [expr2 BSL-Expr])]
Returns 0 if @italic{expr1} or @italic{expr2} are 0, otherwise return -1.

Examples:
@(nested #:style 'code-inset @racketresultblock0[
 > (or0 0 0)

 0

 >(or0 0 400)

 0

 >(or0 200 500)

 -1])

@defform[(and0 expr1 expr2)
         #:contracts
         ([expr1 BSL-Expr]
          [expr2 BSL-Expr])]
Returns 0 if @italic{expr1} and @italic{expr2} are 0, otherwise return -1.

Examples:
@(nested #:style 'code-inset @racketresultblock0[
 > (and0 0 0)

 0

 >(and0 0 400)

 -1

 >(and0 200 500)

 -1])

@defform[(< expr1 expr2)
         #:contracts
         ([expr1 BSL-Expr]
          [expr2 BSL-Expr])]
Returns 0 if @italic{expr1} is less than @italic{expr2}, otherwise return -1.

Examples:
@(nested #:style 'code-inset @racketresultblock0[                          
 > (< 10 10)

 -1
 
 > (< 10 20)

 0


 > (< 20 10)

 -1])

@defform[(> expr1 expr2)
         #:contracts
         ([expr1 BSL-Expr]
          [expr2 BSL-Expr])]
Returns 0 if @italic{expr1} is less than @italic{expr2}, otherwise return -1.

Examples:
@(nested #:style 'code-inset @racketresultblock0[
 > (> 10 10)

 -1
                                                
 > (> 10 20)

 -1

 > (> 20 10)

 0])

@defform[(<= expr1 expr2)
         #:contracts
         ([expr1 BSL-Expr]
          [expr2 BSL-Expr])]
Returns 0 if @italic{expr1} is less than or equal to @italic{expr2}, otherwise return -1.

Examples:
@(nested #:style 'code-inset @racketresultblock0[
 > (<= 10 10)

 0
 
 > (<= 10 20)

 0


 > (<= 20 10)

 -1])

@defform[(>= expr1 expr2)
         #:contracts
         ([expr1 BSL-Expr]
          [expr2 BSL-Expr])]
Returns 0 if @italic{expr1} is greater than or equal to @italic{expr2}, otherwise return -1.
@(nested #:style 'code-inset @racketresultblock0[
 > (>= 10 10)

 0
 
 > (>= 10 20)

 -1


 > (>= 20 10)

 0])

@defform[(+ expr1 expr2)
         #:contracts
         ([expr1 BSL-Expr]
          [expr2 BSL-Expr])]
Adds @italic{expr1} and @italic{expr2} together.

Examples:
@(nested #:style 'code-inset @racketresultblock0[
 > (+ 1 2)

 3

 > (+ 100 100)

 200

 > (+ 0 0)

 0])

@defform[(- expr1 expr2)
         #:contracts
         ([expr1 BSL-Expr]
          [expr2 BSL-Expr])]
Subtracts @italic{expr2} from @italic{expr1}.

@(nested #:style 'code-inset @racketresultblock0[
 > (- 1 2)

 -1

 > (- 100 100)

 0

 > (- 0 0)

 0])


@defform[(* expr1 expr2)
         #:contracts
         ([expr1 BSL-Expr]
          [expr2 BSL-Expr])]
Multiplies @italic{expr1} and @italic{expr2}.

Examples:
@(nested #:style 'code-inset @racketresultblock0[
 > (* 1 2)
             
 2
             
 > (* 100 100)
             
 10000
             
 > (* 0 0)
             
 0])

@defform[(/ expr1 expr2)
         #:contracts
         ([expr1 BSL-Expr]
          [expr2 BSL-Expr])]
Divides @italic{expr2} from @italic{expr2}.

Examples:
@(nested #:style 'code-inset @racketresultblock0[
 > (/ 1 2)

 1/2

 > (/ 100 100)

 1

 > (/ 0 0)

 "invalid number"])

@defform[(add1 expr)
         #:contracts
         ([expr BSL-Expr])]
Adds 1 to @italic{expr}.

Examples:
@(nested #:style 'code-inset @racketresultblock0[
 > (add1 0)

 1

 > (add1 2)

 3

 > (add1 -4)

 -3])

@defform[(sub1 expr)
         #:contracts
         ([expr BSL-Expr])]
Subtracts 1 from expr.

Examples:
@(nested #:style 'code-inset @racketresultblock0[
 > (sub1 0)

 -1

 > (sub1 2)

 1

 > (sub1 -4)

 -5])

@section[#:tag "examples"]{Examples}

@subsection{Rosette Fun}

Here is a really cool example that illustrates how Rosette changes how the language evaluates things.



@racketblock[
 (define-symbolic (func1 c d) (if0 (<= c d) 12 3))
 (define-symbolic (func2 a) (func1 (func1 3 4) a))
 (func2 (func1 x 2))]

Evaluates to
@(nested #:style 'code-inset @racketresultblock0[
 (ite (<= x 2) 12 3)])

While
@racketblock[
 (define-symbolic (func1 c d) (if0 (< c d) 12 3))
 (define-symbolic (func2 a) (func1 (func1 3 4) a))
 (func2 (func1 x 2))]

Evaluates to
@(nested #:style 'code-inset @racketresultblock0[
 3])

Why?

Well, let's walk through what happens.

In the first example, @racket[func2] is given @racket[(func1 x 2)] and so evaluates to

@racketblock[(func1 (func1 3 4) (func1 x 2))]

Then, @racket[(func1 3 4)] and @racket[(func1 x 2)] are evaluated, which results in

@racketblock[(func1 12 (if0 (<= x 2) 12 3))]

Which then evaluates to

@racketblock[(if0 (<= 12 (if0 (<= x 2) 12 3)) 12 3)]

Rosette already knows that the only results that the inner if0 can produce are 12 and 3.
Since 12 could be less than or equal to the result of @racket[(if0 (<= x 2))], Rosette
doesn't pre-emptively know what the result will be, and therefore returns the expanded
Rosette version of @racket[if0], which on the inside is an if-then-else in the form of
@racket[(ite (<= f 2) 12 3)], since it has already figured out that the result of the outside
ite is dependant on, and is essence the same result as what would happen with just
@racket[(<= x 2)], and thus just returns

@racketblock[(ite (<= x 2) 12 3)]

Meanwhile, in the second example, the exact expansion happens, except with @racket[<=] replaced
with @racket[<]. This doesn't chnage anything until

@racketblock[(func1 12 (if0 (< x 2) 12 3))]

Which then evaluates to

@racketblock[(if0 (< 12 (if0 (< x 2) 12 3)) 12 3)]

Rosette already knows that the only results that the inner if0 can produce are 12 and 3.
Since neither 12 nor 3 are strictly @racket[<] than 12, Rosette knows that no matter the result of the
inner @racket[if0], the result will be 3, and therefore returns 3.
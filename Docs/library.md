# Language vs. Library

This is the second in the fourteen part series:

* [Scheme in F#](Docs/intro.md)
* [Just 'let' Me Be!](let.md)
* [Lambda the Ultimate!](lambda.md)
* [Rinse and Recurse](recurse.md)
* [What 'letrec' Can't Do](letstar.md)
* [What's Lisp Without Lists?!](lists.md)
* [No Wait, Macro the Ultimate!](macros.md)
* [Oh, The Humanity!](mutation.md)
* Language vs. Library
* [Turning Your Brain Inside Out With Continuations](continuations.md)
* [Playing Dice with the Universe](amb.md)
* [Functional I/O (or at least "O")](functional_o.md)
* [Functional I/O (including "I" this time)](functional_i.md)
* [Historical Debugging](debugging.md)

## Primitives

Perhaps this post should have gone along with the [one about macros](macros.md) and how Lisp is a “programmable programming language.” The common tension in any language or runtime design is how much to build in as primitives and how much to implement as libraries within the language or atop the runtime. Many of the primitives we’ve already built into the interpreter are really not so primitive at all and can be defined in terms of each other.

For example, we introduced ‘list’ as a convenience to avoid nested ‘cons’. The expression (list a b c) is equivalent to (cons a (cons b (cons c ()))). We could define ‘list’ as a macro in terms of ‘cons’ or maybe in terms of ‘quote’ and ‘eval’. Deciding what subset of Scheme to consider truly primitive, in terms of which seemingly primitive things are defined, is a balancing act. If you really want to go crazy you can ultimately define everything in terms of ‘lambda’ or ‘macro’ (see [this fun post!](http://blogs.msdn.com/b/ashleyf/archive/2008/12/03/the-lambda-calculus.aspx)).

## The Mother ‘load’

Here we’ll build a bit of infrastructure to begin the migration of features out of the language and into libraries. We’ll add a ‘load’ function to allow loading source files and we’ll designate a “Prelude.scm” library to be loaded automatically.

``` fsharp
and load file = (File.OpenText(file)).ReadToEnd() |> 
                List.ofSeq |> parse |> List.iter (eval environment >> ignore)
and Load = function 
    | [String(path)] -> load path; Symbol(sprintf "Loaded '%s'." path)
    | m -> malformed "load" (List(m))
```

This lets us `(load “MySource.scm”)` presumably containing definitions. Besides explicitly loading your own libraries we’ll also add an implicit call to `load "Prelude.scm"` before kicking off the REPL to include library-defined primitive. We should go ahead and quickly add a feature to allow comments (simicolon to end-of-line); just skipping over them during tokenization.

``` fsharp
let tokenize source = 
    let rec comment = function 
        | '\r' :: t | '\n' :: t -> t // terminated by line end 
        | [] -> [] // or by EOF 
        | _ :: t -> comment t 
    ...
let rec tokenize' acc = function 
    ...
    | ';' :: t -> comment t |> tokenize' acc // skip over comments 
```

## Prelude

We can now include in Prelude.scm the boolean operators we used as examples in the [macro post](macros.md):

``` scheme
; logical 'and', 'or', 'not', 'xor' 
(define and (macro (a b) '(if ,a (if ,b 1 0) 0))) 
(define or (macro (a b) '(if ,a 1 (if ,b 1 0)))) 
(define not? (lambda (x) (if x 0 1))) 
(define xor (lambda (a b) (and (or a b) (not? (and a b)))))
```

We can start to really turn this into a proper language with standard higher-order list functions:

``` scheme
; map function (f) over list (xs) 
(define map (lambda (f xs)    ; apply f to each element of xs
    (if xs                    ; if not empty then
        (cons (f (car xs))    ; cons f of the head...
            (map f (cdr xs))) ; onto result of recursing down the tail
        ())))                 ; otherwise return empty

; fold function (f) over list (xs) while accumulating (a) 
(define fold (lambda (f a xs) 
    (if (not? xs) a 
        (fold f (f (car xs) a) (cdr xs))) 
    ))
```

This isn’t really a lesson in higher order functions, but notice how `map` and `fold` abstract over categories of recursion, allowing us to then define other things in terms of them without thinking recursively. For example, to reverse a list:

``` scheme
(define reverse (lambda (xs) (fold cons () xs)))
```

Or, like we promised, we can start replacing built-in primitives such as `list` and then remove them from the F# source:

``` scheme
(define list (macro (xs) '(map eval (quote ,xs))))
```

It’s starting to get really fun!

[One other feature recently added is the ability to pass a variable number of args to a lambda or macro by simply calling with more args than defined params. In this case the last param becomes a list containing the overflow. This is used in the definition of `list`. I didn’t talk about this, but feel free to check the diffs]

## Tests

``` fsharp
case "(and 0 0)" "0" // or (false) 
case "(and 1 0)" "0" // or (false) 
case "(and 0 1)" "0" // or (false) 
case "(and 1 1)" "1" // or (true) 
case "(or 0 0)" "0" // or (false) 
case "(or 1 0)" "1" // or (true) 
case "(or 0 1)" "1" // or (true) 
case "(or 1 1)" "1" // or (true) 
case "(not? 0)" "1" // or (true) 
case "(not? 1)" "0" // or (false) 
case "(xor 0 0)" "0" // xor (false) 
case "(xor 1 0)" "1" // xor (true) 
case "(xor 0 1)" "1" // xor (true) 
case "(xor 1 1)" "0" // xor (false) 
case "(let ((square (lambda (x) (* x x)))) (map square '(1 2 3 4 5 6 7 8 9)))" "(1 4 9 16 25 36 49 64 81)" // mapping 
case "(let ((square (lambda (x) (* x x)))) (map square '(9)))" "(81)" // mapping single 
case "(let ((square (lambda (x) (* x x)))) (map square '()))" "()" // mapping empty 
case "(fold * 1 '(2 3 4 5))" "120" // fold 
case "(reverse '(1 2 3))" "(3 2 1)" // reverse
```

## Next: [Turning Your Brain Inside Out With Continuations](continuations.md)

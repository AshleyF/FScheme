# What's Lisp Without Lists?!

This is the sixth in the fourteen part series:

* [Scheme in F#](Docs/intro.md)
* [Just 'let' Me Be!](let.md)
* [Lambda the Ultimate!](lambda.md)
* [Rinse and Recurse](recurse.md)
* [What 'letrec' Can't Do](letstar.md)
* What's Lisp Without Lists?!
* [No Wait, Macro the Ultimate!](macros.md)
* [Oh, The Humanity!](mutation.md)
* [Language vs. Library](library.md)
* [Turning Your Brain Inside Out With Continuations](continuations.md)
* [Playing Dice with the Universe](amb.md)
* [Functional I/O (or at least "O")](functional_o.md)
* [Functional I/O (including "I" this time](functinal_i.md)
* [Historical Debugging](debugging.md)

## Lists

How could we even be pretending this is Lisp when we have yet to add lists! We need to add the standard `cons`, `car`, and `cdr` for constructing and destructing list structure and we’ll go ahead and add the nice-to-have `list` (to construct lists from a set of arguments without an ugly chain of `cons`). Because of the isomorphism between Scheme and F# lists, these are embarrassingly simple to implement:

``` fsharp
and Cons = function [h; List(t)] -> List(h :: t) | _ -> failwith "Malformed 'cons'." 
and Car = function [List(h :: _)] -> h | _ -> failwith "Malformed 'car'." 
and Cdr = function [List(_ :: t)] -> List(t) | _ -> failwith "Malformed 'cdr'." 
and Lst args = List(args) 
```

It can’t get much easier than that. `Cons` takes an expression and a list and returns a new list with the expression prepended. `Car` returns the head of a list. `Cdr` returns the tail. `Lst` just makes a list. Add them to the environment as usual and we’re done:

``` fsharp
and environment = 
    extend [] [ 
        ...
        "cons", ref (Function(Cons)) 
        "car", ref (Function(Car)) 
        "cdr", ref (Function(Cdr)) 
        "list", ref (Function(Lst))]
```

## Improper Lists

I decided not to implement “improper lists” and the dotted-pair notation. This is where we would allow pairs to contain something other than a list as their `cdr` and allow a syntax like `(1 2 . 3)` to construct such things. I don’t believe that lacking this feature limits our expressiveness as far as data structures at all. You can still build trees and all, no problem. It would complicate things quite a bit and with little benefit. Maybe we’ll revisit this later…

## Tests

``` fsharp
case "(list 1 2 3)" "(1 2 3)" // list 
case "(car (list 1 2 3))" "1" // car 
case "(cdr (list 1 2 3))" "(2 3)" // cdr 
case "(cons 1 (list 2 3))" "(1 2 3)" // cons 
case "(cons 1 (cons 2 (cons 3 ())))" "(1 2 3)" // cons x3 
case "(let ((a 1) (b 2) (c 3)) (list a b c))" "(1 2 3)" // list 
case "(let ((a (list 1 2 3))) (car a))" "1" // car 
case "(let ((a (list 1 2 3))) (cdr a))" "(2 3)" // cdr
```
## Next: [No Wait, Macro the Ultimate!](macros.md)

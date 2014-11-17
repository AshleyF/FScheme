# Oh, The Humanity!

This is the eighth in the fourteen part series:

* [Scheme in F#](intro.md)
* [Just 'let' Me Be!](let.md)
* [Lambda the Ultimate!](lambda.md)
* [Rinse and Recurse](recurse.md)
* [What 'letrec' Can't Do](letstar.md)
* [What's Lisp Without Lists?!](lists.md)
* [No Wait, Macro the Ultimate!](macros.md)
* Oh, The Humanity!
* [Language vs. Library](library.md)
* [Turning Your Brain Inside Out With Continuations](continuations.md)
* [Playing Dice with the Universe](amb.md)
* [Functional I/O (or at least "O")](functional_o.md)
* [Functional I/O (including "I" this time](functinal_i.md)
* [Historical Debugging](debugging.md)

## set, begin, define

Here we implement chapters 10 and 11 of [Bill Hails’ book](http://billhails.net/Book/). We’re about to do something hideous and horrible to the language (and to our interpreter). We’re about to add assignment. This is a travesty to a pure functional programmer, but this whole thing is a learning experience and we’re not going to shy away from exploring many paradigms (I can’t wait to get to Prolog-like logic programming later).

## Assignment

Destructive assignment is done with ‘set!’ (“set bang”) in Scheme. Given a symbol and an expression, it finds the symbol in the environment (potentially walking the frames) and assigns the symbol a new value. It’s the first primitive we have that actually doesn’t return any useful value. To a pure functional programmer this is a very strange function. It takes arguments and returns nothing useful! It’s called for its side effects. In our case, we’ll have it return a dummy value, just for display in the REPL.

``` fsharp
and Set env = function 
    | [Symbol(s); e] –> 
        (lookup env s) := eval env e 
        Dummy(sprintf "Set %s" s) 
    | _ -> failwith "Malformed 'set!'."
```

With this we can say `(set! x 5)` or `(set! x (- 7 2))` and `x` will be bound to `5`; replacing any previous binding. It’s not like nested `let`s with which you can shadow the name `x` with a new binding. This is destructively replacing the binding in place. It’s won’t be restored when the current scope is popped.

## Sequencing

With assignment we are entering the imperative programming paradigm. To a “normal” C# or Java dev, this style of executing a sequence of statements for their side effects seems perfectly natural. In C#, for example, there is no special syntax to construct sequences of statements. Simple juxtaposition implies it, or I suppose you could say the simicolon is kind of the “sequencing operator”. To a pure functional programmer statements (as opposed to expressions) are the most unnatural thing in the world. In Scheme we’ll have to use an explicit sequencing operation called ‘begin’.

``` fsharp
and Begin env = List.fold (fun _ e -> eval env e) (Dummy("Empty 'begin'"))
```

It takes a list of statements/expressions and evaluates them in turn. The return value of the whole expression is that of the last expression in the list. For example, `(begin (* 2 3) (* 4 5))` will yield `20`. What happens to the `(* 2 3)`? Well, it’s evaluated but the `6` is dropped on the floor; no use in that. It’s meant to be used with side-effecting statements like ‘set!’ that are called for their effects, not their values. For example, `(let ((a 1)) (begin (set! a 2) (* a 10)))` yields `20`.

## Definitions

While `set!` works to update existing bindings, `define` is used to update existing environment frames themselves with entirely new name/value pairs. It’s primarily used in top-level code to define a set of library functions. It’s nice also at the REPL to be able to define things and then use them rather that constructing these giant `let` expressions to set everything up for the body to make sense. To allow updating whole frames in place, we had to change them to refs and patch up a few things (the extend and lookup functions, etc.). I’m skipping some details.

``` fsharp
and Define (env : Environment) = function 
    | [Symbol(s); e] –> 
        let def = ref (Dummy("Dummy 'define'")) 
        env.Head := Map.add s def env.Head.Value 
        def := eval env e 
        Dummy(sprintf "Defined %s" s) 
    | _ -> failwith "Malformed 'define'."
```

Something like `letrec`, it first populates the current environment frame with a dummy value, then evaluates the bound expression against this, then swaps in the result. This allows for recursive definitions like `(define fac (lambda (x) (if x (* x (fac (- x 1))) 1)))`.

That’s about it for this set of horrible features. With them we’ve enabled imperative programming but we’ve broken [referential transparency](http://en.wikipedia.org/wiki/Referential_transparency_(computer_science)) and ruined the language. Congrats! :-)

## Tests

``` fsharp
case "(let ((a 1)) (begin (set! a 2) a))" "2" // begin and assign 
case "(let* ((a 5) (dummy (set! a 10))) a)" "10" // re-assign after let 
case "(begin (define fac (lambda (x) (if x (* x (fac (- x 1))) 1))) (fac 7))" "5040" // define recursive 
case "(begin (define square (lambda (x) (* x x))) (square 4))" "16" // global def 
case "(let ((x 4)) (begin (define y 8) (* x y))))" "32" // local def
```

## Next: [Language vs. Library](library.md)

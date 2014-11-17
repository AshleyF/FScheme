# What ‘letrec’ Can’t Do

This is the fifth in the fourteen part series:

* [Scheme in F#](Docs/intro.md)
* [Just 'let' Me Be!](let.md)
* [Lambda the Ultimate!](lambda.md)
* [Rinse and Recurse](recurse.md)
* What 'letrec' Can't Do
* [What's Lisp Without Lists?!](lists.md)
* [No Wait, Macro the Ultimate!](macros.md)
* [Oh, The Humanity!](mutation.md)
* [Language vs. Library](library.md)
* [Turning Your Brain Inside Out With Continuations](continuations.md)
* [Playing Dice with the Universe](amb.md)
* [Functional I/O (or at least "O")](functional_o.md)
* [Functional I/O (including "I" this time](functinal_i.md)
* [Historical Debugging](debugging.md)

In this series I’m glossing over all the gory details. You really should read [Bill Hails’ book](http://billhails.net/Book/)!

## Simultaneous 'let'

We just added `letrec` to solve the issue of bindings referring back to themselves. Remember that to implement it we first extend the environment with a frame containing dummy values, then eval bindings against that dummy frame, and finally swap out the dummy values with actual results. But what happens if the bindings are not lambdas (which promise not to touch the values until later)? Won’t they end up trying to use dummy values before they’re swapped? Consider something like `(letrec ((a 1) (b a)) b)`. Will this work? Won’t `b` refer to a dummy value for `a`?

Well, it turns out that, by sheer luck, our implementation is actually sequentially doing destructive updates to the dummy values so it works as long as there’s no forward refs. Remember this code from `LetRec`:

``` fsharp
let update = function [Symbol(s); e] -> (env'.Head.Item s) := (eval env' e) … 
List.iter update bindings
```

By the time `(b a)` is evaluated, the real value for `a` will indeed be in place. This is pure luck though and isn’t necessarily the intended semantics of `letrec`. I do want to come back at some point and change `letrec` to avoid mutation and this behavior will almost certainly change. Interestingly enough though, even PLT Scheme acts this way.

## Sequential ‘let’

At any rate, we’re faithfully following along with Bill. We plan to introduce `let*` (“let star”) to allow sequential bindings. It frees us to later change `letrec` and it will be nice syntactic sugar to avoid resorting to nested lets to accomplish the same thing: `(let ((a 1)) (let ((b a)) b))`. The implementation is pretty similar to regular `let`:

``` fsharp
and LetStar env = function 
    | [List(bindings); body] –> 
        let bind env = function List([Symbol(s); e]) -> [s, ref (eval env e)] |> extend env 
                       | _ -> failwith "Malformed 'let*' binding." 
        let env' = List.fold bind env bindings 
        eval env' body 
    | _ -> failwith "Malformed 'let*'."
```

Notice how similar the `bind` and `env'` above are to that of normal `let`:

``` fsharp
let bind = function List([Symbol(s); e]) -> s, ref (eval env e) … 
let env' = List.map bind bindings |> extend env
```

Instead of returning a single bound pair, `bind` takes an `env` and returns a new environment, extended with a frame containing the single binding. Then instead of mapping over a list of bindings, we fold over them while threading the environment through. The result is a new environment that’s been extended repeatedly with a chain of frames, each with one binding having been evaluated against the tail built before it.

That’s it. All that’s left is to add our new `let*` to the global environment:

``` fsharp
and environment = 
    extend [] [ 
        ...
        "let*", ref (Special(LetStar))]
```

## Tests

And add a few tests:

``` fsharp
case "(let ((a 1) (b 2)) (let ((a b) (b a)) b))" "1"               // let binds in parallel (should work in earlier versions too)
case "(let ((a 1) (b 2)) (let* ((a b) (b a)) b))" "2"              // let* binds sequentially
case "(let ((a 5)) (let ((b (* a 2))) (let ((c (- b 3))) c)))" "7" // poor-man's sequential expressions
case "(let* ((a 5) (b (* a 2)) (c (- b 3))) c)" "7"                // let* sequential expressions
```
## Next: [What's Lisp Without Lists?!](lists.md)

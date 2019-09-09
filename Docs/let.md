# Just ‘let’ Me Be Already!

This is the second in the fourteen part series:

* [Scheme in F#](intro.md)
* Just 'let' Me Be!
* [Lambda the Ultimate!](lambda.md)
* [Rinse and Recurse](recurse.md)
* [What 'letrec' Can't Do](letstar.md)
* [What's Lisp Without Lists?!](lists.md)
* [No Wait, Macro the Ultimate!](macros.md)
* [Oh, The Humanity!](mutation.md)
* [Language vs. Library](library.md)
* [Turning Your Brain Inside Out With Continuations](continuations.md)
* [Playing Dice with the Universe](amb.md)
* [Functional I/O (or at least "O")](functional_o.md)
* [Functional I/O (including "I" this time)](functional_i.md)
* [Historical Debugging](debugging.md)

Still working through [Bill Hails’ awesome book](http://billhails.net/Book/). Adding to the FScheme interpreter from [the previous post](intro.md). Now we’ll add `let`. To do this we’re changing the evaluation model into an environment-passing one.

## Environment Passing

So now the environment isn’t going to just be a single global map. It’s now going to be a list of maps. Each time a new scope is created, the environment will be extended. When looking up symbols, the whole chain is searched. We’ll add a couple of helper functions for this and the global environment will become an extension of empty (plus we’ll go ahead and add a mapping for our yet-to-be-defined `let`):

``` fsharp
let extend env bindings = (Map.ofList bindings) :: env 
let lookup env symbol = 
    match List.tryPick (Map.tryFind symbol) env with 
    | Some(e) –> e 
    | None -> sprintf "No binding for '%s'." symbol |> failwith
and environment = 
    extend [] [ 
        "*", Function(Multiply) 
        "-", Function(Subtract) 
        "if", Special(If) 
        "let", Special(Let)]
```

Then special forms change to take an environment (which is a `Map<string, Expression>` list):

``` fsharp
type Expression = 
    | Number of BigInteger 
    | String of string 
    | Symbol of string 
    | List of Expression list 
    | Function of (Expression list -> Expression) 
    | Special of (Map<string, Expression> list -> Expression list -> Expression)
```

Then eval and apply change to thread the environment through. Also symbols need to be looked up with our helper and the REPL needs to thread in the global initially (note `env` begin threaded throughout):

``` fsharp
and eval env expression = 
    match expression with 
    | Expression.Number(_) as lit –> lit 
    | Expression.String(_) as lit –> lit 
    | Expression.Symbol(s) -> lookup env s 
    | Expression.List(h :: t) –> 
        match eval env h with 
        | Function(f) -> apply env f t 
        | Special(f) -> f env t 
        | _ -> failwith "Malformed expression." 
    | _ -> failwith "Malformed expression."
and apply env fn args = fn (List.map (eval env) args)
let rep = List.ofSeq >> parse >> List.head >> (eval environment) >> print
```

Finally, we need to continue this plumbing work through `If` which needs to just pass it along to `eval`:

``` fsharp
let rec If env = function 
    | [condition; t; f] –> 
        match eval env condition with 
        | List([]) | String("") -> eval env f // empty list or empty string is false 
        | Number(n) when n = 0I -> eval env f // zero is false 
        | _ -> eval env t // everything else is true 
    | _ -> failwith "Malformed If."
```

## Finally, ‘let’

Sheesh, finally we can implement `let`. We just map the bindings onto a new (extended) environment frame and eval the body within that:

``` fsharp
and Let env = function 
    | [List(bindings); body] –> 
        let bind = function List([Symbol(s); e]) -> s, (eval env e) | _ -> failwith "Malformed 'let' binding." 
        let env' = List.map bind bindings |> extend env 
        eval env' body 
    | _ -> failwith "Malformed Let."
```

## Tests

Add a couple test cases:

``` fsharp
case "(let ((x 2)) x)" "2" // simple let 
case "(let ((a 00) (b 10) (c 20)) (if a b c))" "20" // conditional eval
```

## Next: [Lambda the Ultimate!](lambda.md)

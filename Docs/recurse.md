# Rinse and Recurse

This is the fourth in the fourteen part series:

* [Scheme in F#](Docs/intro.md)
* [Just 'let' Me Be!](let.md)
* [Lambda the Ultimate!](lambda.md)
* Rinse and Recurse
* [What 'letrec' Can't Do](letstar.md)
* [What's Lisp Without Lists?!](lists.md)
* [No Wait, Macro the Ultimate!](macros.md)
* [Oh, The Humanity!](mutation.md)
* [Language vs. Library](library.md)
* [Turning Your Brain Inside Out With Continuations](continuations.md)
* [Playing Dice with the Universe](amb.md)
* [Functional I/O (or at least "O")](functional_o.md)
* [Functional I/O (including "I" this time](functinal_i.md)
* [Historical Debugging](debugging.md)

## Recursive ‘let’

Normal `let` can’t be used to bind names to recursive expressions (ones referring back to the names) because the expressions end up being evaluated in the calling environment before it’s been extended with the names themselves. It’s something of a chicken and egg problem. There are a few ways to solve it but the technique I’ve seen often (in [SICP](http://mitpress.mit.edu/sicp/), [LispKit](http://www.amazon.com/gp/product/0133315797/ref=as_li_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0133315797&linkCode=as2&tag=bookporn-20&linkId=EMSSTA7BLP7RGXTR), [Lisp in Small Pieces](http://www.amazon.com/gp/product/0521545668/ref=as_li_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0521545668&linkCode=as2&tag=bookporn-20&linkId=KKJXEREGJXRVJZKW), and this [Bill Hails book](http://www.amazon.com/gp/product/0521545668/ref=as_li_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0521545668&linkCode=as2&tag=bookporn-20&linkId=KKJXEREGJXRVJZKW) we’re following) is to:

1. Create a dummy environment with names but unusable values
2. Eval expressions in this dummy environment
3. Swap out the dummy values with these actual results

This trick works as long as the expressions don’t actually crack open the dummy values. That is exactly the case with `lambda`. Remember it creates a new function; capturing the environment but not actually using it until the function is called. So we’ll make a new ‘letrec’ for binding recursive lambda expressions.

``` fsharp
and LetRec env = function 
    | [List(bindings); body] –> 
        let dummy = Function(fun _ -> failwith "Cannot evaluate dummy values.") 
        let bind = function List([Symbol(s); _]) -> s, ref dummy | _ -> failwith "Malformed 'letrec' binding." 
        let env' = List.map bind bindings |> extend env 
        // now update dummy env - assumes dummy env will be captured but not actually accessed (e.g. lambda) 
        let update = function List([Symbol(s); e]) -> (env'.Head.Item s) := (eval env' e) 
                     | _ -> failwith "Malformed 'letrec' binding." 
        List.iter update bindings 
        eval env' body 
    | _ -> failwith "Malformed LetRec."
```

Notice that in `update` we have to do a destructive update (with `:=`) to swap out the dummy values! Maybe we’ll come back later and redo without resorting to mutation…

## Mutable Environment

As much as I hate mutation, I think we need it to accomplish the swapping out in step 3 above. There are ways to do it without mutation but it’s by far the most straight forward and will come in handy when we later add ‘set!’ to the language itself. First we’ll go through and change the environment bindings to refs, patch up a couple places to expect that and go ahead and add our new `letrec` to the global environment (note the addition of `ref` through):

``` fsharp
type Expression = 
    | Number of BigInteger 
    | String of string 
    | Symbol of string 
    | List of Expression list 
    | Function of (Expression list -> Expression) 
    | Special of (Map<string, Expression ref> list -> Expression list -> Expression) 
and Let env = function 
    | [List(bindings); body] –> 
        let bind = function List([Symbol(s); e]) -> s, ref (eval env e) | _ -> failwith "Malformed 'let' binding." 
        let env' = List.map bind bindings |> extend env 
        eval env' body 
    | _ -> failwith "Malformed Let."

and Lambda env = function 
    | [List(parameters); body] –> 
        let closure env' args = 
            // bind parameters to actual arguments (evaluated in the caller's environment) 
            let bindings = List.zip parameters args 
            let bind = function Symbol(p), a -> p, ref (eval env' a) | _ -> failwith "Malformed 'lambda' parameter." 
            let env'' = List.map bind bindings |> extend env // extend the captured definition-time environment 
            eval env'' body 
        Special(closure) 
    | _ -> failwith "Malformed Lambda."

and environment = 
    extend [] [ 
        "*", ref (Function(Multiply)) 
        "-", ref (Function(Subtract)) 
        "if", ref (Special(If)) 
        "let", ref (Special(Let)) 
        "letrec", ref (Special(LetRec)) 
        "lambda", ref (Special(Lambda))]

and eval env expression = 
    match expression with 
    | Expression.Number(_) as lit –> lit 
    | Expression.String(_) as lit –> lit 
    | Expression.Symbol(s) -> (lookup env s).Value 
    | Expression.List(h :: t) –> 
        match eval env h with 
        | Function(f) -> apply env f t 
        | Special(f) -> f env t 
        | _ -> failwith "Malformed expression." 
    | _ -> failwith "Malformed expression."
```

## Tests

All the old tests still pass. Just one new one:

``` fsharp
case "(letrec ((factorial (lambda (n) (if n (* n (factorial (- n 1))) 1)))) (factorial 4))" "24" // letrec and recursion
```

## Next: [What 'letrec' Can't Do](letstar.md)

# No Wait, Macro the Ultimate!

This is the seventh in the fourteen part series:

* [Scheme in F#](Docs/intro.md)
* [Just 'let' Me Be!](let.md)
* [Lambda the Ultimate!](lambda.md)
* [Rinse and Recurse](recurse.md)
* [What 'letrec' Can't Do](letstar.md)
* [What's Lisp Without Lists?!](lists.md)
* No Wait, Macro the Ultimate!
* [Oh, The Humanity!](mutation.md)
* [Language vs. Library](library.md)
* [Turning Your Brain Inside Out With Continuations](continuations.md)
* [Playing Dice with the Universe](amb.md)
* [Functional I/O (or at least "O")](functional_o.md)
* [Functional I/O (including "I" this time)](functional_i.md)
* [Historical Debugging](debugging.md)

## The Soul of Scheme

We’re now getting into the language-oriented features of Scheme. This is why Scheme is one of my very favorite languages. Scheme is of course a multi-paradigm language; functional, imperative (next post), object oriented (soon), but most wonderfully and uniquely language-oriented. In object-oriented programming the approach is to extend the type system to your problem domain and then solve the domain-specific problem at hand using these new types. In language-oriented development you first extend the language itself (not just the type system) with domain specific constructs and then solve the problem. All of the recent hype around DSLs is old hat for Lisp guys.

Lisp has been called a “programmable programming language" and this language-oriented approach is what Alan Kay meant when he said, “Lisp isn't a language, it's a building material." I love how Paul Graham phrased the whole creative experience in his book On Lisp: “Language and program evolve together. Like the borders between two warring states, the boundary between language and program is drawn and redrawn, until eventually it comes to rest along the mountains and rivers, the natural frontiers of your problem.”

F# can also be used in a language-oriented style by way of quotations. There's a chapter on the subject in Don Syme's Expert F#. Thomas Petricek has done some amazing work with [Web Tools using F# quotations](http://tomasp.net/blog/fswebtools-intro.aspx). We will be adding quotations to our Scheme implementation in this post. The difference between F# and Scheme quotations though is that in F# a quotation becomes an expression tree; an AST representation normally used internally as output from the parser and input to the compiler. It expresses all of the same basic concepts but in an entirely different structure to that of the surface syntax of the language in which you’re used to working.

Scheme on the other hand can be thought of as having no surface syntax at all! It’s as if the syntax is just a straight serialization of the AST. This makes manipulating program structure just as natural as manipulating data structure. The structure is one and the same. In Scheme, program fragments and data are both just lists and can be tinkered with equally using [`cons`, `car` and `cdr`](lists.md). This one-to-one correspondence between internal program representation and syntactic representation is termed homoiconicity and is a key distinguishing feature of the Lisp family. [McCarthy](http://en.wikipedia.org/wiki/John_McCarthy_(computer_scientist)) originally planned to add [M-expression](http://en.wikipedia.org/wiki/M-expression) syntax on top of S-expressions but never did and honestly people who really use Lisp for serious work don’t want any syntax. People [poke fun at all of the parenthesis](http://xkcd.com/297/) but they’re there for good reason.

## Quotations

To enable working with program fragments as data, we’ll first introduce ‘quote’ which will be a special form that returns its argument as plain data without evaluating. Additionally, we’ll allow for marking pieces within a quoted structure with ‘unquote’ which will cause just those pieces to be evaluated within.

``` fsharp
and Quote env = 
    let rec unquote = function 
        | List([Symbol("unquote"); e]) -> eval env e 
        | List(Symbol("unquote") :: _) -> failwith "Malformed 'unquote'." // too many args 
        | List(h :: t) -> List(h :: (List.map unquote t)) // recurse 
        | e –> e 
function [e] -> unquote e | _ -> failwith "Malformed 'quote'."
```

This lets us do things like `(quote (* 2 (- 5 2)))` and instead of getting `6`, we get nested list structure containing symbols and numbers; literally: `(* 2 (- 5 2))`. If we want to evaluation just part of it we could say `(quote (* 2 (unquote (- 5 2))))` and get `(* 2 3)`. Notice the `unquote` is more like a keyword that only has meaning within a quoted expression.

Using `quote` and `unquote` is extremely common and could use a more optimized syntax. Yes, I did just say “syntax.” This is one place where we will consider breaking the homoiconicity and introduce special syntax. Internally (in the AST), we will still have `quote` and `unquote` symbols in function application form (and you’re free to stick with that), but the syntax will be simplified to an apostrophe or comma preceding expressions to be quoted or unquoted respectively. This can be handled at lexing/parsing time. First add to the tokenizer:

``` fsharp
type Token = 
    | Open | Close 
    | Quote | Unquote // <-- added
    | Number of string 
    | String of string 
    | Symbol of string
let rec tokenize' acc = function 
    ...
    | ',' :: t -> tokenize' (Unquote :: acc) t // <-- added
    | '"' :: t -> // start of string           // <-- added
```

Then the parser. We factor our the Open match a bit and reuse for `Quote :: Open` and `Unquote :: Open`:

``` fsharp
let rec list f t acc = 
    let e, t' = parse' [] t 
    parse' (List(f e) :: acc) t' 
and parse' acc = function 
    ...
    | Quote :: Open :: t -> list (fun e -> [Symbol("quote"); List(e)]) t acc 
    | Quote :: h :: t -> parse' (List([Symbol("quote"); map h]) :: acc) t 
    | Unquote :: Open :: t -> list (fun e -> [Symbol("unquote"); List(e)]) t acc 
    | Unquote :: h :: t -> parse' (List([Symbol("unquote"); map h]) :: acc) t 
```

Now instead of the heavy looking `(quote (* 2 (unquote (- 5 2))))`, we can say `‘(* 2 ,(- 5 2))` and yield the same `(* 2 3)`. Or how about `(let ((x 'rain) (y 'spain) (z 'plain)) '(the ,x in ,y falls mainly on the ,z))`. What do you think that resolves to? It’s much nicer looking syntax indeed.

## Eval

Quotations are useful for manipulating program structure as lists but then we need a way of explicitly executing our manipulations. This is where `eval` comes in. The semantics are a little tricky. First it evaluates a quotation against the passed in environment (the call-time environment). The result of that will be the literal expression (with unquoted pieces resolved). Then that is evaluated again but in the global environment:

``` fsharp
and Eval env = function [args] -> args |> eval env |> eval environment | _ -> failwith "Malformed 'eval'."
```

So now we can say `(eval ‘(* 2 ,(- 5 2)))` and get `6`.

## Macros

Now for the grand finale. We want to be able to introduce our own special forms into the language; to program the programming language. It is true that [`lambda`](lambda.md) allows us to add things that look like first class primitives; nearly indistinguishable from built-in functions. But there are limitations.

For example, let’s try to build `and` which will take two arguments and return true (`1`) if both are true. The catch is that it should have the same semantics of the real `and` which would be a special form that doesn’t evaluate its arguments up front but instead “short circuits” in that if the first argument evaluates to false it shouldn’t bother to evaluate the second. This would be partially for efficiency but more importantly to maintain workable semantics. What if, for example, the second argument were a recursive call and the `and` expression was the base case? Eagerly evaluating could cause an infinite loop!

Using only `lambda`, we could try `(let ((and (lambda (a b) (if a (if b 1 0) 0)))) (and 0 BOOM))`. Oops, we see an error (No binding for 'BOOM'.) even though it should not have touched that, once it realized that the first argument in `(and 0 BOOM)` is false (`0`). The `if` is indeed a special form and short-circuits, but the issue is that all of the arguments to `and` are evaluated before calling the lambda. That’s the nature of applicative order languages (lazy languages like Haskell need no “special form” distinction).

Let’s try again. This time we’ll quote the arguments and explicitly eval them within the lambda: `(let ((and (lambda (a b) (if (eval a) (if (eval b) 1 0) 0)))) (and '0 'BOOM))`. Well that works (we get `0` and no complaint about missing bindings) but how very annoying to expose the implementation details to the callers. We’d much rather the quoting at the call site to be implicit. Enter `macro`:

``` fsharp
and Macro env = function 
    | [List(parameters); body] –> 
        let closure env' args = 
            // bind parameters to actual arguments (but unevaluated, unlike lambda) 
            let bindings = List.zip parameters args 
            let bind = function Symbol(p), a -> p, ref a | _ -> failwith "Malformed 'macro' parameter." 
            let env'' = List.map bind bindings |> extend env // extend the captured definition-time environment 
            eval env'' body |> eval env' // eval against bound args, then again in the caller's environment 
        Special(closure) 
    | _ -> failwith "Malformed 'macro'."
```

This is much the same as Lambda but it binds the arguments in a new environment frame without evaluating them in the calling environment. It then evaluates the body in that extended environment as usual. Then finally evaluates the result against the captured environment. In other words, a macro’s arguments are treated as implicitly quoted and are bound as literals (without evaluating). The result of evaluating the body is expected to be a modified program structure that’s then evaluated again to get a result.

Now we can rewrite our `and` as `(let ((and (macro (a b) '(if ,a (if ,b 1 0) 0)))) (and 0 BOOM))`. Notice that the macro uses quoting and unquoting internally but callers can treat `and` exactly as if it were a primitive special form. By the way, we can define `or` as well with similar short-circuit semantics: `(let ((or (macro (a b) '(if ,a 1 (if ,b 1 0))))) (or 1 BOOM))`. Pretty neat stuff.

One of the super-grand finales of the whole series will be to come back and replace a bunch of our primitive special forms (written in F#) with a library written in FScheme itself!

# Tests

``` fsharp
case "(quote (* 2 3))" "(* 2 3)" // quote primitive 
case "'(* 2 3)" "(* 2 3)" // quote primitive with sugar 
case "(eval '(* 2 3))" "6" // eval quoted expression 
case "(quote (* 2 (- 5 2)))" "(* 2 (- 5 2))" // quote nested 
case "(quote (* 2 (unquote (- 5 2))))" "(* 2 3)" // quote nested unquote 
case "'(* 2 ,(- 5 2))" "(* 2 3)" // quote nested unquote with sugar 
case "(quote (quote 1 2 3))" "(quote 1 2 3)" // quote special form 
case "(let ((x 'rain) (y 'spain) (z 'plain)) '(the ,x in ,y falls mainly on the ,z))" 
"(the rain in spain falls mainly on the plain)" // quote/unquote 
case "(let ((* -)) (eval '(* 3 3)))" "9" // eval uses top-level environment 
case "(let ((or (macro (a b) '(if ,a 1 (if ,b 1 0))))) (or 1 BOOM))" "1" // macro as special form 
case "(let ((and (macro (a b) '(if ,a (if ,b 1 0) 0)))) (and 0 BOOM))" "0" // macro as special form
```

## Next: [Oh, The Humanity!](mutation.md)

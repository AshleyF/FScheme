# Turning Your Brain Inside Out with Continuations

This is the tenth in the fourteen part series:

* [Scheme in F#](Docs/intro.md)
* [Just 'let' Me Be!](let.md)
* [Lambda the Ultimate!](lambda.md)
* [Rinse and Recurse](recurse.md)
* [What 'letrec' Can't Do](letstar.md)
* [What's Lisp Without Lists?!](lists.md)
* [No Wait, Macro the Ultimate!](macros.md)
* [Oh, The Humanity!](mutation.md)
* [Language vs. Library](library.md)
* Turning Your Brain Inside Out With Continuations
* [Playing Dice with the Universe](amb.md)
* [Functional I/O (or at least "O")](functional_o.md)
* [Functional I/O (including "I" this time](functional_i.md)
* [Historical Debugging](debugging.md)

We’re into one of the most magical chapters in [Bill Hails’ book](http://billhails.net/Book/). We’re about to add a very strange and dangerous feature to the interpreter: `call/cc` (“call with current continuation”). To appreciate the beauty of it, we’ll first look at continuation passing style (CPS) in general, what it’s good for, then how `call/cc` works, some of the implementation details, and finally how it’s used in Scheme.

## Continuation Passing Style

Wes Dyer has a [nice post on CPS](http://blogs.msdn.com/wesdyer/archive/2007/12/22/continuation-passing-style.aspx). The concept is closely related to tail call optimization (TCO) which we [just talked about in the last post](library.md) (if you haven’t read it, you should and then continue here – ha!). The idea of CPS is that instead of functions returning values to their callers, they’re passed a continuation function to be called with what would have been the return value. That is, functions never return! No really, they never ever return! This is shocking enough that in Bill’s book he leaves a blank page “to allow time for reflection on the enormity of the previous statement.” It is indeed an astounding way to program! Once you’re comfortable with the idea of tail recursion though, this idea should at least not seem totally infeasible.

Our running example is the standard factorial function:

``` fsharp
let rec fac n = if n = 0 then 1 else n * fac (n - 1)
```

However `fac 100000000` causes a `StackOverflowException`. To solve this we used an accumulation parameter (`a`) and made it properly tail recursive:

``` fsharp
let rec fac n a = if n = 0 then a else fac (n - 1) (n * a)
```

CPS is very similar but instead of passing forward an accumulated result, we pass a closure capturing the current result. This closure becomes the new continuation; expecting to be passed the next result which may recurse again with a new closure, etc.

``` fsharp
let rec fac n c = if n = 0 then c 1 else fac (n - 1) (fun n' -> c (n * n'))
```

These closures of closures of closures are finally invoked when the base case is hit. We can try the above by passing in an initial continuation that simply prints the result:

``` fsharp
fac 100000000 (printfn "%i")
```

It’s interesting that this is no longer blows the stack but instead it causes an OutOfMemoryException! The call is certainly tail recursive, but the continuation itself is accreting continuations of continuations of continuations. For CPS to be viable, we need to use a form of TCO that doesn’t accrete state. The following works:

``` fsharp
let rec fac n a c = if n = 0 then c a else fac (n - 1) (n * a) c
```

This is really a mechanical transformation of the original TCO version.

Side note: Luckily for us, F# supports TCO. In other languages it may be necessary to implement something like the trampoline in his book. The idea is to not call continuations directly, but instead to always call a central function with a thunk that when invoked will call the continuation. This keeps the stack from growing and actually has a side benefit of centralizing all continuation dispatch which allows for some cool tricks. Maybe I’ll write another post on the trampoline later.

## Transforming To CPS

Transforming functions to CPS is something like folding your code inside out and it feels like the same is happening to your brain. Monads can help turn it back to normal but that’s a separate topic. If you’re interested though, notice that the [Monadic Coconuts post from a while back](http://blogs.msdn.com/ashleyf/archive/2009/12/14/monadic-piles-of-coconuts.aspx) was essentially using continuations and F#’s monadic computation expression syntax to make the ugly CPS look pretty again. Continuations are at the heart of monads.

Let’s practice some CPS conversions. A function returning a constant value is simplest of all:

``` fsharp
let greeting () = "hello"
```

Becomes:

``` fsharp
let greeting () c = c "hello"
```

Passing a continuation (`c`) and call it with what would have been the return value. A function performing a simple computation is no more difficult:

``` fsharp
let add a b = a + b
```

Becomes:

``` fsharp
let add a b c = c (a + b)
```

Again just passing in a continuation which is called with the result. What might not be obvious is how to get the result and do something with it; maybe print it. Just as we did with the factorial example, we pass in a print function as the initial continuation and it’s called with the return value:

``` fsharp
> add 2 3 (printfn "%i") 
5
```

Where CPS starts to get complicated is with nested function calls. Take this pair of non-CPS functions:

``` fsharp
let double x = 2 * x 
let square x = x * x
```

And call them from a third function:

``` fsharp
let sqrdbl x = double (square x)
```

Calling `square` and passing the result to `double`.

Side note: This could have been written using composition as `let sqrdbl = double >> square` You'll notice that when we convert to CPS we lose this compositionality. Using monads gives CPS compositionality again. I just found that interesting...

Converting `double` and `square` to CPS is simple enough:

``` fsharp
let double x c = c (2 * x) 
let square x c = c (x * x)
```

But converting `sqrdbl` involves turning things inside out:

``` fsharp
let sqrdbl x c = square x (fun y -> double y c)
```

First, `sqrdbl` now takes a continuation. It passes `x` to square as usual but can’t just pass the continuation (`c`) along to square. Instead we have to make a new closure (capturing ‘`c`’) that will be called with the square of `x`. This new continuation will then forward that on to `double` and, since `double` is the last in the sequence, pass on the original continuation which will be called with the ultimate result.

``` fsharp
> sqrdbl 7 (printfn "%i")
98
```

As you can imagine, deeply nested function calls can result in very confusing CPS transformations that tie your brain in knots. Believe it or not, we intend to transform the whole guts of the FScheme interpreter into CPS. Fun!

## What is CPS Good For Anyway?

Before we transform the whole interpreter and introduce `call/cc,` let’s see if we can get a glimpse of what CPS can do for us. Here’s a simple example that hopefully shows some of the power. First we define a silly little set of functions named ‘`a`’, ‘`b`’, ‘`c`’ and ‘`d`’ that print their names and call a passed in continuation:

``` fsharp
let a cont = printf "A, "; cont () 
let b cont = printf "B, "; cont () 
let c cont = printf "C, "; cont () 
let d cont = printf "D, "; cont ()
```

Calling a sequence of these in CPS would look like:

``` fsharp
let demo () = a (fun _ -> b (fun _ -> c (fun _ -> d (fun _ -> printfn "Done"))))
```

We can try it out in the interactive:

``` fsharp
> demo ();; 
A, B, C, D, Done
```

Nothing interesting yet; just calls each in turn, printing “A, B, C, D, Done”. What may not be apparent is that what we’ve done is really give up control over the flow of the sequence. For example, if we redefine ‘`b`’ to no longer call its continuation:

``` fsharp
let b cont = printf "B (terminate!)"
```

The same demo function now produces:

``` fsharp
> demo ();; 
A, B (terminate!)
```

This is something like throwing an exception and bailing out. In other words, we could use CPS to build the mechanics of structured exception handling if we needed. For example, we could define a continuation-based exception handling mechanism in which you always pass in two continuations; one to be called upon success and another (representing the ‘catch’ block) to be called upon error.

## The True Essence

A very strange thing to realize about continuations is that they are essentially closures that represent all that is needed to complete the computation. That is, they are self-contained units that represent the rest of the computation; a very strange idea. You’re free to store them away and use them elsewhere. I hope you’re getting the same sense of eureka that I got when first realizing what this really means! Say we redefine ‘b’ to not just terminate, but to store away the current continuation and then fall through without continuing:

``` fsharp
let mutable resume = (fun () -> printfn "Replace me!") 
let b cont = printf "B (suspend!)"; resume <- cont
```

The result is, as expected:

``` fsharp
> demo ();; 
A, B (suspend!)
```

But now think about what `resume` represents. It contains the rest of the computation. It could easily represent some arbitrarily complicated mess as long as the mess is expressed in CPS. Now we can:

``` fsharp
> resume ();; 
C, D, Done
```

Picking up where we left off. In fact we’re free to resume as many times as we like and, assuming pure (mutation free) code, get the same result each time.

## One Crazy Idea (of Many)

You could imagine doing some crazy things with this. For example, we could suspend the sequence and kick off an asynchronous call to some web service and then resume upon callback. Normally, writing async code is a kludge with begin/end async calls all over and having to separate code into callback functions. Notice that we could do this automagically without changing the original definition of the `demo` function at all. The guy writing ‘demo’ could think of his code as a synchronous sequence and we could handle the async mess under the covers. This is actually very close to how async workflows work in F#.

Plenty of other crazy ideas come to mind. What if we pass a pair of producer/consumer or generator/iterator continuations back and forth, creating coroutines? Or what if you could serialize continuations and ship them off to a different machine on which to resume? So many bizarre things are possible.

## Converting to a CPS-based Interpreter

Honestly, transforming the interpreter to CPS was a bit of a pain. I’m not going to go into all the details but you can check the diffs.

First, a continuation is just an Expression -> Expression function. We want to change Function and Special to take a continuation argument and add the idea of an expression resolving to a continuation:

``` fsharp
type Expression = 
    | Number of BigInteger 
    | String of string 
    | Symbol of string 
    | List of Expression list 
    | Function of (Continuation -> Expression list -> Expression) 
    | Special of (Continuation -> Environment -> Expression list -> Expression) 
    | Current of Continuation 
    | Dummy of string 
and Continuation = Expression -> Expression
```

For many parts of the interpreter the transformation is pretty straight forward. For example, `cons`, `car` and `cdr` simply take a continuation and forward their return value to it (note the addition of `cont`):

``` fsharp
and Cons cont = function [h; List(t)] -> (List(h :: t)) |> cont | …
and Car cont = function [List(h :: _)] -> h |> cont | …
and Cdr cont = function [List(_ :: t)] -> List(t) |> cont | …
```

The more complicated transformations were places where we were using maps and folds. Perhaps I should have written a general CPS version of List.map, List.fold, etc. As just one example, ‘Begin’ was a pretty simple fold:

``` fsharp
and Begin env = List.fold (fun _ e -> eval env e) (Dummy("Empty 'begin'"))
```

And ended up turning into the more complicated version below; explicitly handling the recursion, creating a new continuation and handing off to the passed in continuation in the base case:

``` fsharp
and Begin cont env = 
    let rec foldeval last = function 
        | h :: t -> eval (fun x -> foldeval x t) env h 
        | [] -> last |> cont 
    foldeval (Dummy("Empty 'begin'"))
```

We had to do something similar everywhere we were doing maps and folds. I don’t want to bore you with the mostly mechanical details. You can check the diffs or can imagine how it’s possible from the transformation samples we did earlier. We painstakingly went through and converted absolutely everything in the interpreter to CPS.

Side note: We also added some math functions, added ‘display’ for printing expressions as a side effect, and added escaped whitespace characters to string literal parsing, added a ‘while’ loop macro to the prelude, etc. But I want to stick to the topic at hand.

## Two Worlds Intersect

Like quantum entanglement or something, we're about to allow the language and the internals of the language implementation to intertwine.

We’ve gone to all of this work rewriting the interpreter using CPS everywhere, what difference does it make from the Scheme author’s perspective? The underlying mechanics have changed out from under them but the behavior of running Scheme programs hasn’t changed at all.

As far as the interpreter, we could use the fact that we’re internally using CPS to now build exception handling for example. We could add ‘try’ and ‘catch’ keywords to the interpreter and the implementation would be simple given our new architecture. We could imagine adding an async-workflows-like feature or threading or coroutines of any number of other new features, now made easier by having converted everything to CPS. All of these would be features added as new primitives to the interpreter itself.

However, remember all the talk about how the [soul of Scheme is as a programmable programming language](macros.md)? The big idea is to imagine exposing the CPS mechanics to the running programs. Programs execute in one world while the interpreter is implemented in another. We want to add a single small hook joining the two worlds in a controlled way. It’s a very dangerous feature and could easily be abused. The point though is that Scheme is not meant to be the direct implementation language for a problem domain. Instead, it’s expected that a domain specific language will be build atop this language kernel. This shows the minimalistic nature of Scheme. Rather than build in various controlled features such as structured exception handling or yielding or threading, we build in the single essence of all of them and leave it up to the DSL author to implement as a library.

## Call with Current Continuation

The new primitive we will add is ‘call/cc’. It takes a lambda as an argument and passes to it the current continuation from the interpreter’s world. The implications of this are somewhat mind blowing! First, here’s the implementation:

``` fsharp
and CallCC cont env = function 
    | [callee] -> eval (function Special(fn) -> fn cont env [Current(cont)] 
                                 | m -> malformed "call/cc" m) env callee 
    | m -> malformed "call/cc" (List(m))
```

Pretty innocent looking. It just takes a continuation (as does everything now) and expects a `callee` which should be a lambda expecting one argument. It then bundles the continuation from the interpreter’s world up as a Current expression and passes it to the lambda in the Scheme world. We’ve just handed over enormous power!

Let’s try just a few ideas to get the wheels spinning. We can use it to abort computations by just defining a function that when called will return to the top-level:

``` scheme
; simple continuation to top-level 
(define escape nil) 
(call/cc (lambda (c) (set! escape c)))
```

This stores away the current (top-level) continuation as ‘escape’ which can now be called from some arbitrarily deeply nested expression, causing whatever is happening to abort (regular continuation is never called). To make it a bit more useful we can define an error function that prints a message and then escapes:

``` scheme
; error mechanism - print message and break out to top-level 
(define error (lambda (msg) (begin (display msg) (escape nil))))
```

We can now use this to abort sequences:

``` scheme
> (begin (display "A, ") (display "B, ") (error "Opps!") (display "C")) 
A, B, Opps!
```

Or we can do the pause/resume trick from earlier:

``` scheme
(define resume nil) 
(define pause (lambda () (call/cc (lambda (k) (begin (set! resume k) (escape nil))))))
(begin (pause) (display 'A) (pause) (display 'B) (pause) (display 'C))
```

Then:

``` scheme
> (resume nil) 
A 
> (resume nil) 
B 
> (resume nil) 
C
```

Pretty neat!

In fact, here’s a really cool idea: We can simulate threading by storing away multiple paused continuations and then resuming them round robin. First:

``` scheme
(define t0 nil) 
(define t1 nil) 
(define pause (macro (t) '(call/cc (lambda (k) (begin (set! ,t (lambda () (k nil))) (escape nil))))))
```

Wrapping in a macro lets us pass in a thread name:

``` scheme
(begin (pause t0) (display 'A) 
(pause t0) (display 'B) 
(pause t0) (display 'C))
(begin (pause t1) (display 1) 
(pause t1) (display 2) 
(pause t1) (display 3))
```

Now we can interleave the sequences very easily and get “A1B2C3”:

``` scheme
(t0)(t1)(t0)(t1)(t0)(t1)
```

Each sequence is defined independently of the other and the thread scheduling is defined separately (we could build further infrastructure to maintain a list of threads and have a proper scheduler; all in Scheme). We’re well on our way to building a sort of [green threads](http://en.wikipedia.org/wiki/Green_threads) library. More importantly, we now have the power to build any kind of control flow we like into the language. Pretty potent feature!

## Next: [Playing Dice with the Universe](amb.md)

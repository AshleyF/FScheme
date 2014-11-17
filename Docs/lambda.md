# Lambda the Ultimate!

This is the third in the fourteen part series:

* [Scheme in F#](intro.md)
* [Just 'let' Me Be!](let.md)
* Lambda the Ultimate!
* [Rinse and Recurse](recurse.md)
* [What 'letrec' Can't Do](letstar.md)
* [What's Lisp Without Lists?!](lists.md)
* [No Wait, Macro the Ultimate!](macros.md)
* [Oh, The Humanity!](mutation.md)
* [Language vs. Library](library.md)
* [Turning Your Brain Inside Out With Continuations](continuations.md)
* [Playing Dice with the Universe](amb.md)
* [Functional I/O (or at least "O")](functional_o.md)
* [Functional I/O (including "I" this time](functional_i.md)
* [Historical Debugging](debugging.md)

Continuing along in [Bill Hails’ book](http://billhails.net/Book/v2.html). Be sure to follow the [previous posts](let.md).

## Lambda

Now we’re going to add the all-powerful ‘lambda’! Since we’ve already done all the environment-passing plumbing from the last post, this will be straight forward. Lambda creates closures (read [Bill’s book](http://billhails.net/Book/v2.html) for an excellent description with pictures).

Essentially, it returns a new function that when called will do the following:

1. Create a new environment frame, binding parameter to call-time arguments which are evaluated in the **caller’s environment**
2. Extend the captured **definition-time environment** with this new frame
3. Finally, eval the body in this extended environment

``` fsharp
and Lambda env = function 
    | [List(parameters); body] –> 
        let closure env' args = 
            // bind parameters to actual arguments (evaluated in the caller's environment) 
            let bindings = List.zip parameters args 
            let bind = function Symbol(p), a -> p, (eval env' a)
                       | _ -> failwith "Malformed 'lambda' parameter." 
            let env'' = List.map bind bindings |> extend env // extend the captured env 
            eval env'' body 
        Special(closure) 
    | _ -> failwith "Malformed Lambda."
```

When Lambda is called, it’s given the definition-time environment as env. We capture this by way of the inner ‘closure’ function immediately returned as a Function(closure) Expression. Notice that ‘closure’ itself takes the call-time environment as env’ (with a prime mark). Later, when the function happens to be called (potentially in a completely different scope) it is passed the caller’s environment and does it’s magic.

We just need to add this guy to the global environment as a special form and we’re done:

``` fsharp
and environment = 
    extend [] [ 
    ...
    "lambda", Special(Lambda)]
```

If you want to marvel at the beauty and power of lambda, [check out this post](http://blogs.msdn.com/ashleyf/archive/2008/12/03/the-lambda-calculus.aspx) :-)

## Tests

As usual, more tests:

``` fsharp
case "(let ((square (lambda (x) (* x x)))) (square 4))" "16" // lambda 
case "(let ((square (lambda (x) (* x x)))) square)" "Function" // print lambda 
case "(let ((times3 (let ((n 3)) (lambda (x) (* n x))))) (times3 4))" "12" // closure 
case "(let ((times3 (let ((makemultiplier (lambda (n) (lambda (x) (* n x))))) (makemultiplier 3)))) (times3 5))" "15" // higher order functions
```

## Next: [Rinse and Recurse](recurse.md)

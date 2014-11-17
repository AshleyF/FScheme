# FScheme - Scheme in F#

This is the beginning of a fourteen part series:

* Scheme in F#
* [Just 'let' Me Be!](let.md)
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
* [Functional I/O (including "I" this time](functinal_i.md)
* [Historical Debugging](debugging.md)

One of my New Year’s goals is to re-read [Lisp in Small Pieces](http://www.amazon.com/gp/product/0521545668/ref=as_li_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0521545668&linkCode=as2&tag=bookporn-20&linkId=PMDZINVZLBD65EE4) and implement all 11 interpreters and 2 compilers. As much as I like the "Lisp in Lisp" idea and enjoyed the eureka moment in SICP when Sussman writes the metacircular interpreter on the board to the music from Space Odyssey, I don't want to do Lisp in Lisp itself. Lisp in F# sounds like more fun.

As a warm-up, I’m going through [Bill Hails’ absolutely awesome book](http://billhails.net/Book/) where he builds “PScheme”; a Scheme interpreter in Perl. Of course, doing it in F#. Here’s v0.0.0. It turned out pretty small (about 100 lines):

## Tokenizer

I kind of hate these kind of state machine parsers. Later I’ll come back and redo this as parser combinators or lex/yacc, etc. but for now it’s simple enough (just strings, numbers and symbols):

``` fsharp
open System 
open System.Numerics 

type Token = 
    | Open | Close 
    | Number of string 
    | String of string 
    | Symbol of string

let tokenize source = 
    let rec string acc = function 
        | '\\' :: '"' :: t -> string (acc + "\"") t // escaped quote becomes quote 
        | '"' :: t -> acc, t // closing quote terminates 
        | c :: t -> string (acc + (c.ToString())) t // otherwise accumulate chars 
        | _ -> failwith "Malformed string." 
    let rec token acc = function 
        | (')' :: _) as t -> acc, t // closing paren terminates 
        | w :: t when Char.IsWhiteSpace(w) -> acc, t // whitespace terminates 
        | [] -> acc, [] // end of list terminates 
        | c :: t -> token (acc + (c.ToString())) t // otherwise accumulate chars 
    let rec tokenize' acc = function 
        | w :: t when Char.IsWhiteSpace(w) -> tokenize' acc t // skip whitespace 
        | '(' :: t -> tokenize' (Open :: acc) t 
        | ')' :: t -> tokenize' (Close :: acc) t 
        | '"' :: t -> // start of string 
            let s, t' = string "" t 
            tokenize' (Token.String(s) :: acc) t' 
        | '-' :: d :: t when Char.IsDigit(d) -> // start of negative number 
            let n, t' = token ("-" + d.ToString()) t 
            tokenize' (Token.Number(n) :: acc) t' 
        | '+' :: d :: t | d :: t when Char.IsDigit(d) -> // start of positive number 
            let n, t' = token (d.ToString()) t 
            tokenize' (Token.Number(n) :: acc) t' 
        | s :: t -> // otherwise start of symbol 
            let s, t' = token (s.ToString()) t 
            tokenize' (Token.Symbol(s) :: acc) t' 
        | [] -> List.rev acc // end of list terminates 
    tokenize' [] source
```

## Parser

The near-equivalence between syntax and semantics in Scheme makes the parser trivial. There’s almost a 1:1 correspondence between tokens and expressions. One exception is the `Open`/`Close` tokens denote Expression lists:

``` fsharp
type Expression = 
    | Number of BigInteger 
    | String of string 
    | Symbol of string 
    | List of Expression list 
    | Function of (Expression list -> Expression) 
    | Special of (Expression list -> Expression) 

let parse source = 
    let map = function 
        | Token.Number(n) -> Expression.Number(BigInteger.Parse(n)) 
        | Token.String(s) -> Expression.String(s) 
        | Token.Symbol(s) -> Expression.Symbol(s) 
        | _ -> failwith "Syntax error." 
    let rec parse' acc = function 
        | Open :: t –> 
            let e, t' = parse' [] t 
            parse' (List(e) :: acc) t' 
        | Close :: t -> (List.rev acc), t 
        | h :: t -> parse' ((map h) :: acc) t 
        | [] -> (List.rev acc), [] 
    let result, _ = parse' [] (tokenize source) 
    result
```

## Printer

For printing expressions to the console:

``` fsharp
let rec print = function 
    | List(list) -> "(" + String.Join(" ", (List.map print list)) + ")" 
    | String(s) –> 
        let escape = String.collect (function '"' -> "\\\"" | c -> c.ToString()) // escape quotes 
        "\"" + (escape s) + "\"" 
    | Symbol(s) –> s 
    | Number(n) -> n.ToString() 
    | Function(_) | Special(_) -> "Function"
```

## Primitives

We begin with just a few primitives (which are simply Expression –> Expression functions) for doing basic math and conditionals and seed a global environment with them:

``` fsharp
let Multiply args = 
    let prod a = function Number(b) -> a * b | _ -> failwith "Malformed multiplication argument."  
    Number(List.fold prod 1I args)

let Subtract = function 
    | [] -> Number(0I) // (-) == 0 
    | [Number(n)] -> Number(-n) // (- a) == –a 
    | Number(n) :: ns -> // (- a b c) == a - b – c 
        let sub a = function Number(b) -> a - b | _ -> failwith "Malformed subtraction argument." 
        Number(List.fold sub n ns) 
    | _ -> failwith "Malformed subtraction."

let rec If = function 
    | [condition; t; f] –> 
        match eval condition with 
        | List([]) | String("") -> eval f // empty list or empty string is false 
        | Number(n) when n = 0I -> eval f // zero is false 
        | _ -> eval t // everything else is true 
    | _ -> failwith "Malformed 'if'."

and environment = 
    Map.ofList [ 
        "*", Function(Multiply) 
        "-", Function(Subtract) 
        "if", Special(If)]
```

## Eval/Apply

The classic yin/yang of eval/apply are very easy to implement. Literals eval to themselves, symbols are looked up in the environment, and lists are applied. Special forms are, well, special in that they don’t eval their arguments up front; leaving it up to the callee (e.g. `If` will eval one of two expressions depending on the conditional).

Notice that `If` and `environment` above along with eval and apply below are all mutually recursive:

``` fsharp
and eval expression = 
    match expression with 
    | Number(_) as lit –> lit 
    | String(_) as lit –> lit 
    | Symbol(s) -> environment.[s]  
    | List([]) –> List([]) 
    | List(h :: t) –>  
        match eval h with 
        | Function(f) -> apply f t 
        | Special(f) -> f t  
        | _ -> failwith "Malformed expression." 
    | _ -> failwith "Malformed expression."

and apply fn args = fn (List.map eval args)
```

## REPL

The REPL now just reads a line from the console, parses, evals and prints it, then rinses and repeats.

``` fsharp
let rep = List.ofSeq >> parse >> List.head >> eval >> print

let rec repl output = 
    printf "%s\n> " output 
    try Console.ReadLine() |> rep |> repl 
    with ex -> repl ex.Message
```

The whole deal is kicked off with:

``` fsharp
repl "Welcome to FScheme"
```

## Tests

Gotta have tests:

``` fsharp
let test () = 
    let case source expected = 
        try 
            let output = rep source 
            if output <> expected then 
                printf "TEST FAILED: %s (Expected: %s, Actual: %s)" source expected output 
        with _ -> printf "TEST CRASHED: %s" source 
    case "1" "1"                     // numbers
    case "+1" "1"                    // explicit positive numbers
    case "-1" "-1"                   // negative numbers
    case "\"hello\"" "\"hello\""     // strings
    case "(*)" "1"                   // multiplication
    case "(* 2 3)" "6"               // multiplication
    case "(* 2 3 4)" "24"            // multiplication
    case "(-)" "0"                   // strange subtraction case
    case "(- 10)" "-10"              // negation
    case "(- 10 2)" "8"              // subtraction
    case "(- 10 2 3)" "5"            // subtraction
    case "(if (* 0 1) 10 20)" "20"   // if
    case "(if (* 1 1) 10 20)" "10"   // if
    case "(if (* 1 1) 10 bomb)" "10" // if (special form)
    case "(* 1234567890987654321 1234567890987654321)" "1524157877457704723228166437789971041" // bigint math
```
## Next: [Just 'let' Me Be!](let.md)

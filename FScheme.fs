// Copyright 2010 Ashley Nathan Feniello

module FScheme

open System
open System.Numerics
open System.IO

type Token =
    | Open | Close
    | Quote | Unquote
    | Number of string
    | String of string
    | Symbol of string

let tokenize source =
    let rec string acc = function
        | '\\' :: '"' :: t -> string (acc + "\"") t // escaped quote becomes quote
        | '\\' :: 'b' :: t -> string (acc + "\b") t // escaped backspace
        | '\\' :: 'f' :: t -> string (acc + "\f") t // escaped formfeed
        | '\\' :: 'n' :: t -> string (acc + "\n") t // escaped newline
        | '\\' :: 'r' :: t -> string (acc + "\r") t // escaped return
        | '\\' :: 't' :: t -> string (acc + "\t") t // escaped tab
        | '\\' :: '\\' :: t -> string (acc + "\\") t // escaped backslash
        | '"' :: t -> acc, t // closing quote terminates
        | c :: t -> string (acc + (c.ToString())) t // otherwise accumulate chars
        | _ -> failwith "Malformed string."
    let rec comment = function
        | '\r' :: t | '\n' :: t -> t // terminated by line end
        | [] -> [] // or by EOF
        | _ :: t -> comment t
    let rec token acc = function
        | (')' :: _) as t -> acc, t // closing paren terminates
        | w :: t when Char.IsWhiteSpace(w) -> acc, t // whitespace terminates
        | [] -> acc, [] // end of list terminates
        | c :: t -> token (acc + (c.ToString())) t // otherwise accumulate chars
    let rec tokenize' acc = function
        | w :: t when Char.IsWhiteSpace(w) -> tokenize' acc t // skip whitespace
        | '(' :: t -> tokenize' (Open :: acc) t
        | ')' :: t -> tokenize' (Close :: acc) t
        | '\'' :: t -> tokenize' (Quote :: acc) t
        | ',' :: t -> tokenize' (Unquote :: acc) t
        | ';' :: t -> comment t |> tokenize' acc // skip over comments
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
and Frame = Map<string, Expression ref> ref
and Environment = Frame list

let parse source =
    let map = function
        | Token.Number(n) -> Expression.Number(BigInteger.Parse(n))
        | Token.String(s) -> Expression.String(s)
        | Token.Symbol(s) -> Expression.Symbol(s)
        | _ -> failwith "Syntax error."
    let rec list f t acc =
        let e, t' = parse' [] t
        parse' (List(f e) :: acc) t'
    and parse' acc = function
        | Open :: t -> list id t acc
        | Close :: t -> (List.rev acc), t
        | Quote :: Open :: t -> list (fun e -> [Symbol("quote"); List(e)]) t acc
        | Quote :: h :: t -> parse' (List([Symbol("quote"); map h]) :: acc) t
        | Unquote :: Open :: t -> list (fun e -> [Symbol("unquote"); List(e)]) t acc
        | Unquote :: h :: t -> parse' (List([Symbol("unquote"); map h]) :: acc) t
        | h :: t -> parse' ((map h) :: acc) t
        | [] -> (List.rev acc), []
    let result, _ = parse' [] (tokenize source)
    result

let rec print = function
    | List(Dummy(_) :: _) -> "" // don't print accumulated statement dummy values
    | List(list) -> "(" + String.Join(" ", (List.map print list)) + ")"
    | String(s) | Symbol(s) -> s
    | Number(n) -> n.ToString()
    | Function(_) | Special(_) | Current(_) -> "Function"
    | Dummy(_) -> "" // sometimes useful to emit value for debugging, but normally we ignore

let malformed n e = sprintf "Malformed '%s': %s" n (print (List([e]))) |> failwith

let math ident unary op name cont = function
    | [] -> Number(ident) |> cont // (op) == 0
    | [Number(n)] -> Number(unary * n) |> cont // (op a) == -a or +a
    | Number(n) :: ns -> // (op a b c) == a op b op c
        let op' a = function Number(b) -> op a b | m -> malformed (sprintf "%s arg" name) m
        Number(List.fold op' n ns) |> cont
    | m -> malformed name (List(m))

let Add = math 0I 1I (+) "addition"
let Subtract = math 0I -1I (-) "subtraction"
let Multiply = math 1I 1I (*) "multiplication"
let Divide = math 1I 1I (/) "division"
let Modulus = math 1I 1I (%) "modulus"

let compare pred name cont = function
    | [Number(a); Number(b)] -> (if pred a b then Number(1I) else Number(0I)) |> cont
    | m -> malformed name (List(m))

let Equal = compare (=) "equality"
let Greater = compare (>) "greater"
let Less = compare (<) "less"

let extend env bindings = (ref (Map.ofList bindings) :: env)
let lookup env symbol =
    match List.tryPick (fun (frame : Frame) -> Map.tryFind symbol frame.Value) env with
    | Some(e) -> e
    | None -> sprintf "No binding for '%s'." symbol |> failwith

let zip args parameters =
    let args' = // passing more args than params results in last param treated as list
        let plen = List.length parameters
        if List.length args = plen then args else
            let split ts = ts (plen - 1) args |> List.ofSeq
            split Seq.take @ [List(split Seq.skip)]
    List.zip parameters args'

let mutable backtrack = [] // ambivalent back stack

let rec If cont env = function
    | [condition; t; f] ->
        eval (function
            | List([]) | String("") -> eval cont env f // empty list or empty string is false
            | Number(n) when n = 0I -> eval cont env f // zero is false
            | _ -> eval cont env t) env condition // everything else is true
    | m -> malformed "if" (List(m))

and Let cont env = function
    | [List(bindings); body] ->
        let rec mapbind acc = function
            | List([Symbol(s); e]) :: t -> eval (fun x -> mapbind ((s, ref x) :: acc) t) env e
            | [] ->
                let frame = List.rev acc
                let env' = extend env frame
                eval cont env' body
            | _ -> failwith "Malformed 'let' binding."
        mapbind [] bindings
    | m -> malformed "let" (List(m))

and LetRec cont env = function
    | [List(bindings); body] ->
        let bind = function List([Symbol(s); _]) -> s, ref (Dummy("Dummy 'letrec'")) | m -> malformed "letrec binding" m
        let env' = List.map bind bindings |> extend env
        let frame = env'.Head.Value
        // now update dummy env - assumes dummy env will be captured but not actually accessed (e.g. lambda)
        let rec mapupdate = function
            | List([Symbol(s); e]) :: t -> eval (fun x -> (frame.Item s) := x; mapupdate t) env' e
            | [] -> eval cont env' body
            | _ -> failwith "Malformed 'let' binding."
        mapupdate bindings
    | m -> malformed "letrec" (List(m))

and LetStar cont env = function
    | [List(bindings); body] ->
        let rec foldbind env' = function
            | List([Symbol(s); e]) :: t -> eval (fun x -> foldbind ([s, ref x] |> extend env') t) env' e
            | [] ->eval cont env' body
            | _ -> failwith "Malformed 'let*' binding."
        foldbind env bindings
    | m -> malformed "let*" (List(m))

and Lambda cont env = function
    | [List(parameters); body] ->
        let closure cont' env' args =
            // bind parameters to actual arguments (evaluated in the caller's environment)
            let rec mapbind acc = function
                | (Symbol(p), a) :: t -> eval (fun x -> mapbind ((p, ref x) :: acc) t) env' a
                | [] ->
                    let env'' = List.rev acc |> extend (env @ env') // extend the captured definition-time environment
                    eval cont' env'' body
                | _ -> failwith "Malformed lambda param."
            mapbind [] (zip args parameters)
        Special(closure) |> cont
    | m -> malformed "lambda" (List(m))

and Cat cont = function [List(a); List(b)] -> List(a @ b) |> cont | m -> malformed "cat" (List(m))
and Cons cont = function [h; List(t)] -> (List(h :: t)) |> cont | m -> malformed "cons" (List(m))
and Car cont = function [List(h :: _)] -> h |> cont | m -> malformed "car" (List(m))
and Cdr cont = function [List(_ :: t)] -> List(t) |> cont | m -> malformed "cdr" (List(m))

and Quote cont env =
    let rec unquote cont' = function
        | List([Symbol("unquote"); e]) -> eval cont' env e
        | List(Symbol("unquote") :: _) as m -> malformed "unquote (too many args)" m
        | List(lst) ->
            let rec mapunquote acc = function
                | h' :: t' ->
                    unquote (fun x -> mapunquote (x :: acc) t') h'
                | [] -> List(List.rev acc)
            mapunquote [] lst |> cont'
        | e -> cont' e
    function [e] -> unquote cont e | m -> malformed "quote" (List(m))

and Eval cont env = function [args] -> args |> eval (eval cont env) env | m -> malformed "eval" (List(m))

and Macro cont env = function
    | [List(parameters); body] ->
        let closure cont' env' args =
            // bind parameters to actual arguments (but unevaluated, unlike lambda)
            let bind = function Symbol(p), a -> p, ref a | _, m -> malformed "macro parameter" m // bound unevaluated
            let env'' = zip args parameters |> List.map bind |> extend env // extend the captured definition-time environment
            eval (eval cont' env') env'' body
        Special(closure) |> cont
    | m -> malformed "macro" (List(m))

and Set cont env = function
    | [Symbol(s); e] -> eval (fun x -> (lookup env s) := x; Dummy(sprintf "Set %s" s) |> cont) env e
    | m -> malformed "set!" (List(m))

and Begin cont env =
    let rec foldeval last = function
        | h :: t -> eval (fun x -> foldeval x t) env h
        | [] -> last |> cont
    foldeval (Dummy("Empty 'begin'"))
    
and Define cont (env : Environment) = function
    | [Symbol(s); e] ->
        let def = ref (Dummy("Dummy 'define'"))
        env.Head := Map.add s def env.Head.Value
        eval (fun x -> def := x; Dummy(sprintf "Defined %s" s) |> cont) env e
    | m -> malformed "define" (List(m))

and load file = Load (fun _ -> Dummy("")) [String(file)] |> ignore
and Load cont = function
    | [String(file)] ->
        (File.OpenText(file)).ReadToEnd() |> List.ofSeq |> parse |> List.iter (eval (fun _ -> Dummy("Dummy 'load'")) environment >> ignore)
        Symbol(sprintf "Loaded '%s'." file) |> cont
    | m -> malformed "load" (List(m))

and Display cont = function
    | [e] -> print e |> printf "%s"; Dummy("Dummy 'display'") |> cont
    | m -> malformed "display" (List(m))

and CallCC cont env = function
    | [callee] -> eval (function Special(fn) -> fn cont env [Current(cont)] | m -> malformed "call/cc" m) env callee
    | m -> malformed "call/cc" (List(m))

and Ambivalent cont env args =
    match args with
    | choice :: t ->
        backtrack <- (fun () -> Ambivalent cont env t) :: backtrack
        eval cont env choice
    | [] ->
        match backtrack with
        | back :: t -> backtrack <- t; back ()
        | [] -> printfn "No more solutions."; Dummy("Unsolvable") |> cont

and environment =
    [ref (Map.ofList
       ["*", ref (Function(Multiply))
        "/", ref (Function(Divide))
        "%", ref (Function(Modulus))
        "+", ref (Function(Add))
        "-", ref (Function(Subtract))
        "=", ref (Function(Equal))
        ">", ref (Function(Greater))
        "<", ref (Function(Less))
        "if", ref (Special(If))
        "let", ref (Special(Let))
        "letrec", ref (Special(LetRec))
        "let*", ref (Special(LetStar))
        "lambda", ref (Special(Lambda))
        "cat", ref (Function(Cat))
        "cons", ref (Function(Cons))
        "car", ref (Function(Car))
        "cdr", ref (Function(Cdr))
        "quote", ref (Special(Quote))
        "eval", ref (Special(Eval))
        "macro", ref (Special(Macro))
        "set!", ref (Special(Set))
        "begin", ref (Special(Begin))
        "define", ref (Special(Define))
        "load", ref (Function(Load))
        "display", ref (Function(Display))
        "call/cc", ref (Special(CallCC))
        "amb", ref (Special(Ambivalent))
        ])]

and eval cont env expression =
    match expression with
    | Number(_) | String(_) | Current(_) as lit -> lit |> cont
    | Symbol(s) -> (lookup env s).Value |> cont
    | List(h :: t) ->
        eval (function
            | Function(f) -> apply cont env f t
            | Special(f) -> f cont env t
            | Current(f) -> match t with [rtn] -> f rtn | m -> malformed "call/cc args" (List(m))
            | m -> malformed "expression" m) env h
    | Dummy(s) -> sprintf "Cannot evaluate dummy value: %s" s |> failwith
    | _ -> failwith "Malformed expression."

and apply cont env fn args =
    let rec mapeval acc = function
        | h :: t -> eval (fun a -> mapeval (a :: acc) t) env h
        | [] -> fn cont (List.rev acc)
    mapeval [] args

let rep env =
    let eval' = function
        | Symbol("?") ->
            match backtrack with
            | h :: t -> backtrack <- t; h ()
            | [] -> printfn "No current problem."; Dummy("No problem")
        | e -> eval id env e
    List.ofSeq >> parse >> List.head >> eval' >> print

let rec repl output =
    printf "%s\n> " output
    try Console.ReadLine() |> rep environment |> repl
    with ex -> repl ex.Message

let test () =
    let case source expected =
        try
            let output = rep environment source
            if output <> expected then
                printf "TEST FAILED: %s [Expected: %s, Actual: %s]" source expected output
        with ex -> printf "TEST CRASHED: %s [%s]" ex.Message source
    case "\"hello\"" "hello" // strings
    case "\"\\\"\"" "\"" // return char
    case "\"\\b\"" "\b" // return char
    case "\"\\f\"" "\f" // return char
    case "\"\\n\"" "\n" // return char
    case "\"\\r\"" "\r" // return char
    case "\"\\t\"" "\t" // return char
    case "\"\\\\\"" "\\" // return char
    case "1" "1" // numbers
    case "+1" "1" // explicit positive numbers
    case "-1" "-1" // negative numbers
    case "(*)" "1" // multiplication
    case "(* 2)" "2" // multiplication
    case "(* 2 3)" "6" // multiplication
    case "(* 2 3 4)" "24" // multiplication
    case "(/)" "1" // division
    case "(/ 2)" "2" // division
    case "(/ 9 2)" "4" // division
    case "(/ 12 2 3)" "2" // division
    case "(%)" "1" // modulus
    case "(% 2)" "2" // modulus
    case "(% 9 2)" "1" // modulus
    case "(% 8 2)" "0" // modulus
    case "(% 26 7 3)" "2" // modulus
    case "(+)" "0" // strange addition case
    case "(+ 10)" "10" // explicit positive
    case "(+ 10 2)" "12" // addition
    case "(+ 10 2 3)" "15" // addition
    case "(-)" "0" // strange subtraction case
    case "(- 10)" "-10" // negation
    case "(- 10 2)" "8" // subtraction
    case "(- 10 2 3)" "5" // subtraction
    case "(if (* 0 1) 10 20)" "20" // if
    case "(if (* 1 1) 10 20)" "10" // if
    case "(if (* 1 1) 10 bomb)" "10" // if (special form)
    case "(* 1234567890987654321 1234567890987654321)" "1524157877457704723228166437789971041" // bigint math
    case "(let ((x 2)) x)" "2" // simple let
    case "(let ((a 00) (b 10) (c 20)) (if a b c))" "20" // conditional eval
    case "(let ((square (lambda (x) (* x x)))) (square 4))" "16" // lambda
    case "(let ((square (lambda (x) (* x x)))) square)" "Function" // print lambda
    case "(let ((times3 (let ((n 3)) (lambda (x) (* n x))))) (times3 4))" "12" // closure
    case "(let ((times3 (let ((makemultiplier (lambda (n) (lambda (x) (* n x))))) (makemultiplier 3)))) (times3 5))" "15" // higher order functions
    case "(letrec ((factorial (lambda (n) (if n (* n (factorial (- n 1))) 1)))) (factorial 4))" "24" // letrec and recursion
    case "(let ((a 1) (b 2)) (let ((a b) (b a)) b))" "1" // let binds in parallel (should work in earlier versions too)
    case "(let ((a 1) (b 2)) (let* ((a b) (b a)) b))" "2" // let* binds sequentially
    case "(let ((a 5)) (let ((b (* a 2))) (let ((c (- b 3))) c)))" "7" // poor-man's sequential expressions
    case "(let* ((a 5) (b (* a 2)) (c (- b 3))) c)" "7" // let* sequential expressions
    case "(list 1 2 3)" "(1 2 3)" // list
    case "(car (list 1 2 3))" "1" // car
    case "(cdr (list 1 2 3))" "(2 3)" // cdr
    case "(cat '(1 2) '(a b))" "(1 2 a b)" // cat
    case "(cat '(1 2) '())" "(1 2)" // cat
    case "(cat '() '(1 2))" "(1 2)" // cat
    case "(cons 1 (list 2 3))" "(1 2 3)" // cons
    case "(cons 1 (cons 2 (cons 3 nil)))" "(1 2 3)" // cons x3
    case "(let ((a 1) (b 2) (c 3)) (list a b c))" "(1 2 3)" // list
    case "(let ((a (list 1 2 3))) (car a))" "1" // car
    case "(let ((a (list 1 2 3))) (cdr a))" "(2 3)" // cdr
    case "(quote (* 2 3))" "(* 2 3)" // quote primitive
    case "'(* 2 3)" "(* 2 3)" // quote primitive with sugar
    case "(eval '(* 2 3))" "6" // eval quoted expression
    case "(quote (* 2 (- 5 2)))" "(* 2 (- 5 2))" // quote nested
    case "(quote (* 2 (unquote (- 5 2))))" "(* 2 3)" // quote nested unquote
    case "'(* 2 ,(- 5 2))" "(* 2 3)" // quote nested unquote with sugar
    case "(quote (quote 1 2 3))" "(quote 1 2 3)" // quote special form
    case "(let ((x 'rain) (y 'spain) (z 'plain)) '(the ,x in ,y falls mainly on the ,z))"
         "(the rain in spain falls mainly on the plain)" // quote/unquote
    case "(let ((or (macro (a b) '(if ,a 1 (if ,b 1 0))))) (or 1 BOOM))" "1" // macro as special form
    case "(let ((and (macro (a b) '(if ,a (if ,b 1 0) 0)))) (and 0 BOOM))" "0" // macro as special form
    case "(let ((a 1)) (begin (set! a 2) a))" "2" // begin and assign
    case "(let* ((a 5) (dummy (set! a 10))) a)" "10" // re-assign after let
    case "(begin (define fac (lambda (x) (if x (* x (fac (- x 1))) 1))) (fac 7))" "5040" // define recursive
    case "(begin (define square (lambda (x) (* x x))) (square 4))" "16" // global def
    case "(let ((x 4)) (begin (define y 8) (* x y))))" "32" // local def
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
    case "(call/cc (lambda (c) (c 10)))" "10" // super-simple call/cc
    case "(call/cc (lambda (c) (if (c 10) 20 30)))" "10" // call/cc bailing out of 'if'
    case "(+ 8 (call/cc (lambda (k^) (* (k^ 5) 100))))" "13" // call/cc bailing out of multiplication
    case "(* (+ (call/cc (lambda (k^) (/ (k^ 5) 4))) 8) 3)" "39" // call/cc nesting

# Functional I/O (Historical Debugging)

This is the last in the fourteen part series:

* [Scheme in F#](Docs/intro.md)
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
* [Functional I/O (including "I" this time)](functional_i.md)
* Historical Debugging

[Historical Debugging introduced in VS2010](http://channel9.msdn.com/posts/VisualStudio/Historical-Debugger-and-Test-Impact-Analysis-in-Visual-Studio-Team-System-2010/) is quite the engineering feat! We can actually pull off something similar in our interpreter with amazing ease. Because of the pure nature of our functional I/O system (last [two](functional_o.md) [posts](functional_i.md)) and the already centrally managed world state and input it’s very straight forward to implement.

## Adding a Historian

First we need to keep a history of the world. We’ll use a double linked list so we can easily walk backward and forward in time:

``` fsharp
let mutable history = new LinkedList<Expression>() 
let mutable current = new LinkedListNode<Expression>(Symbol("Dummy world")) 
```

On each `tick` we’ll append to the history. We’ll also allow stopping and restarting of the passage of time (with a ‘running’ flag). We’re blindly tracking each entire world state. You can imagine a more memory-efficient implementation that only tracks input history and replays forward from the initial state or that maintains only “key frames” of computed world state and partially replays forward as needed. That would be interesting to build but let’s start with the simplest thing that could possibly work:

``` fsharp
let form () = 
    let f = new Form(Text = "Canvas", Width = w * s + 16 - 1, Height = h * s + 38 - 1, Visible = true) 
    let running = ref false 
    let time slice = 
        running := false 
        if slice <> null then 
            current <- slice 
            paint (); f.Refresh() 
    let debug = function 
        | Keys.S -> running := false 
        | Keys.G -> running := true 
        | Keys.Left -> time current.Previous 
        | Keys.Right -> time current.Next 
        | Keys.W -> print current.Value |> printfn "World: %s" 
        | _ –> () 
    let t = new Thread(new ThreadStart(fun () –> 
        while true do 
            if running.Value then 
                current <- history.AddAfter(current, eval' "tick") 
                paint (); f.Refresh() 
        Thread.Sleep(33))) 
    t.Start() 
    f.Paint.Add(fun a -> lock bmp (fun () -> a.Graphics.DrawImage(bmp, 0, 0))) 
    f.MouseMove.Add(fun a -> mouseX := a.X / s; mouseY := a.Y / s) 
    f.Closing.Add(fun _ -> t.Abort()) 
    f.KeyDown.Add(fun a -> debug a.KeyCode) 
    current <- history.AddFirst(eval' "init") 
    running := true 
    f
```

The system behaves just as it always has; ticking off the progress of time upon being (run). But now we can press ‘S’top or ‘G’o to suspend things, we can press/hold left and right arrow keys to move back and forth through history, and we can print the current world state with ‘W’.

Now you can cheat at the [“Pong” game from last post](functional_i.md). If you miss the ball, just back up and try again; rewriting history!

Also, you might notice bugs in the implementation. For example the ball appears to go off the screen when it bounces. You can flip through the history to just before and after it happens and look at the world state. Sure enough, it goes one pixel off before reversing directions… great. Jump on GitHub and fix it! :-)

The beauty of debugging this style of functional I/O code is that any bug is sure to be found in the transition from one state to the next. The 'tick' function, given a particular state, is returning the wrong thing. Just pin down which transition is broken and fix the logic or assumption in the 'tick'. If you're into TDD then first add a unit test passing the particular state in and asserting the expected result. Pure functions are so very much easier to test and debug.

[Side note: I want to plug [another paper worth reading](http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=3041C7F2E6F57CAE760DCAA30CFD96FC?doi=10.1.1.49.695&rep=rep1&type=pdf) if you're interested in functional I/O systems.]

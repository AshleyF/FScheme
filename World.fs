module World

open System.Drawing
open System.Windows.Forms
open System.Numerics
open System.Threading
open System.Collections.Generic
open FScheme

let mutable history = new LinkedList<Expression>()
let mutable current = new LinkedListNode<Expression>(Symbol("Dummy world"))

let mouseX = ref 0
let mouseY = ref 0
let events () = List([Number(bigint mouseX.Value); Number(bigint mouseY.Value)])

let eval' name = eval id environment (List([Symbol(name); List([Symbol("quote"); List([current.Value; events ()])])]))

let w = 32
let h = 32
let s = 8 // pixel size
let bmp = new Bitmap(w * s, h * s)
let paint () = lock bmp (fun () ->
    use gc = Graphics.FromImage(bmp)
    gc.Clear(Color.Black) |> ignore
    match eval' "draw" with
    | List(pixels) ->
        let fill = function
            | List([List([Number(x); Number(y)]); List([Number(r); Number(g); Number(b)])]) ->
                gc.FillRectangle(new SolidBrush(Color.FromArgb(0xFF, int r, int g, int b)), int x * s, int y * s, s - 1, s - 1)
            | _ -> ()
        List.iter fill pixels
    | _ -> failwith "Malformed graphical output.")
    
type Form() =
    inherit System.Windows.Forms.Form()
    override x.OnPaintBackground _ = () // no flicker
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
        | _ -> ()
    let t = new Thread(new ThreadStart(fun () ->
        while true do
            if running.Value then
                current <- history.AddAfter(current, eval' "tick")
                paint (); f.Refresh()
            Thread.Sleep(33)))
    t.Start()
    f.Paint.Add(fun a -> lock bmp (fun () -> a.Graphics.DrawImage(bmp, 0, 0)))
    f.MouseMove.Add(fun a -> mouseX := a.X / s; mouseY := a.Y / s)
    f.KeyDown.Add(fun a -> debug a.KeyCode)
    f.Closing.Add(fun _ -> t.Abort())
    current <- history.AddFirst(eval' "init")
    running := true
    f

let run cont _ = Application.Run(form ()); Dummy("Dummy 'run'.") |> cont

let init () = environment.Head := environment.Head.Value.Add("run", ref (Function(run))) 
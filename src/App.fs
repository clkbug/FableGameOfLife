module Color.Fountain

// Color Fountain by Erik Novales: https://github.com/enovales

open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Browser

let canvas = (document.getElementsByTagName "canvas").[0] :?> HTMLCanvasElement

let width = 100
let height = 80
let cellsize = 10

canvas.width <- width * cellsize |> float
canvas.height <- height * cellsize |> float

let ctx = canvas.getContext_2d()

let rng(): float = JS.Math.random()

type Cell =
    { mutable isAlive: bool }

let makeColor (r: int) (g: int) (b: int) =
    let ($) = fun s n -> s + n.ToString()
    "rgb(" $ r $ ", " $ g $ ", " $ b $ ")"

let black = makeColor 0 0 0
let white = makeColor 255 255 255

let initField() = Array.init height (fun i -> Array.init width (fun j -> { isAlive = rng() < 0.5 }))

let drawField (field: Cell array array) =
    for i in 0 .. (height - 1) do
        for j in 0 .. (width - 1) do
            ctx.fillStyle <- !^(if field.[i].[j].isAlive then black else white)
            ctx.fillRect (j * cellsize |> float, i * cellsize |> float, float cellsize, float cellsize)

let updateField (field: Cell array array) =
    let prevField = Array.init height (fun i -> Array.init width (fun j -> { isAlive = field.[i].[j].isAlive }))
    for i in 0 .. (height - 1) do
        for j in 0 .. (width - 1) do
            let neighborCount =
                Array.filter
                    (fun (dx, dy) ->
                        let x, y = (j + dx + width) % width, (i + dy + height) % height in prevField.[y].[x].isAlive)
                    [| (-1, -1)
                       (-1, 0)
                       (-1, 1)
                       (0, -1)
                       (0, 1)
                       (1, -1)
                       (1, 0)
                       (1, 1) |]
                |> Array.length

            let current = prevField.[i].[j].isAlive
            field.[i].[j].isAlive <- (neighborCount = 3) || (neighborCount = 2 && current)

let timestep = 0.8

let main() =
    let mutable field = initField()
    let mutable isStopped = false
    let button = (document.getElementById "reset-button") :?> HTMLButtonElement
    button.onclick <-
        fun _ ->
            (field <- initField()
             drawField field)
    let button = (document.getElementById "start-button") :?> HTMLButtonElement
    button.onclick <- fun _ -> isStopped <- false
    let button = (document.getElementById "stop-button") :?> HTMLButtonElement
    button.onclick <- fun _ -> isStopped <- true
    canvas.onclick <-
        fun me ->
            let x, y = int me.offsetX / cellsize, int me.offsetY / cellsize
            field.[y].[x].isAlive <- not (field.[y].[x]).isAlive

    let rec loop last t =
        // Comment out this line to make sure the animation runs
        // with same speed on different frame rates
        // let timestep = (t - last) / 20.
        if not isStopped then
            ctx.clearRect (0., 0., 10000., 10000.)
            updateField field
        else
            ()
        drawField field
        window.requestAnimationFrame (loop t) |> ignore

    // start the loop
    loop 0. 0.

main()

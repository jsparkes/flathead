module Graphics

open System
open System.Runtime.InteropServices

open Type
open Utility

type status = {
    mouse_x : int;      (*	    X coordinate of the mouse    *)
    mouse_y : int;      (*	    Y coordinate of the mouse    *)
    button : bool;      (*	    true if a mouse button is pressed    *)
    keypressed : bool;  (*	    true if a key has been pressed    *)
    key : char;         (*	    the character for the key pressed    *)
}

type t = { Graphics : status}

type Event = 
    | Key_pressed
    | Button_down
    | Poll

let mutable original_size = (0, 0)

// Luckily we don't need many colors
let black = ConsoleColor.Black
let blue = ConsoleColor.Blue
let white = ConsoleColor.White
let background = ConsoleColor.White
let foreground = ConsoleColor.Black

let set_color color = Console.ForegroundColor <- color
let set_foreground color = Console.ForegroundColor <- color
let set_background color = Console.BackgroundColor <- color

let open_graph arg = 
    //    FreeConsole() |> ignore
    //    AllocConsole() |> ignore
    //    console <- GetStdHandle(STD_OUTPUT_HANDLE)
    //
    //    let font_size = GetConsoleFontSize(console, 0)
    //    let width = font_size.x
    //    let height = font_size.y
    //    ConsoleColor.
    original_size <- (Console.WindowWidth, Console.WindowHeight)
    set_background background
    set_foreground foreground
    Console.SetWindowSize(130, 55)
    Console.Clear()

let close_graph() = 
    let (w, h) = original_size
    Console.SetWindowSize(w, h)
let moveto x y = 
    Console.SetCursorPosition(x, Console.WindowHeight - y)

let fill_rect x y w h = 
    moveto x y
    Console.Write(Array.create w ' ')

let draw_string (str : string) = 
    if str.Length + Console.CursorLeft >= Console.WindowWidth then
        let sub = left_string str (Console.WindowWidth - Console.CursorLeft - 1)
        Console.Write(sub)
    else
        Console.Write(str)
// We don't measure in pixels
let text_size (str : string) = (1, str.Length)
let set_font name = ()
let auto_synchronize flag = ()
let synchronize() = ()

let wait_next_event (events : List<Event>) = 
    if List.contains Event.Poll events then
        let st = { mouse_x = 0; mouse_y = 0; button = false; keypressed = Console.KeyAvailable; key = '\000'}
        { Graphics = st }
    else
        let k = Console.ReadKey(true)
        let st = { mouse_x = 0; mouse_y = 0; button = false; keypressed = true; key = k.KeyChar}
        { Graphics = st }
        
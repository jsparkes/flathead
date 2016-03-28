module Graphics

open System
open System.Runtime.InteropServices

open Type

type status = {
    mouse_x : int;      (*	    X coordinate of the mouse    *)
    mouse_y : int;      (*	    Y coordinate of the mouse    *)
    button : bool;      (*	    true if a mouse button is pressed    *)
    keypressed : bool;  (*	    true if a key has been pressed    *)
    key : char;         (*	    the character for the key pressed    *)
}

type t = { Graphics : status}

// Emulate the OCaml Graphics packages using Win32 Console functions.
// This is sufficient for Z-Machine files.
// Unfortunately imperative due to inexperience and wanting to maintain
// compatiblity with OCaml code.
//[<StructLayout(LayoutKind.Sequential)>]
type COORD = 
    struct
        val x : Int16
        val y : Int16
    end

// http://pinvoke.net/default.aspx/kernel32/AllocConsole.html
[<DllImport("kernel32", SetLastError = true)>]
extern bool AllocConsole()

// http://pinvoke.net/default.aspx/kernel32/FreeConsole.html
[<DllImport("kernel32.dll", SetLastError = true, ExactSpelling = true)>]
extern bool FreeConsole()

// http://pinvoke.net/default.aspx/kernel32/GetConsoleFontSize.html
[<DllImport("kernel32.dll", SetLastError = true)>]
extern COORD GetConsoleFontSize(IntPtr hConsoleOutput, Int32 nFont)

// http://pinvoke.net/default.aspx/kernel32/GetStdHandle.html
[<DllImport("kernel32.dll", SetLastError = true)>]
extern IntPtr GetStdHandle(int nStdHandle)

let mutable (console : IntPtr) = (IntPtr 0)

let mutable original_size = (0, 0)
let STD_INPUT_HANDLE = -10
let STD_OUTPUT_HANDLE = -11
let STD_ERROR_HANDLE = -12

type Event = 
    | Key_pressed
    | Button_down
    | Poll


// Luckily we don't need many colors
let black = ConsoleColor.Black
let blue = ConsoleColor.Blue
let white = ConsoleColor.White
let background = ConsoleColor.White
let foreground = ConsoleColor.Black
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
    Console.SetWindowSize(80, 50)

let close_graph() = 
    let (w, h) = original_size
    Console.SetWindowSize(w, h)
let set_color color = Console.ForegroundColor <- color
let set_foreground color = Console.ForegroundColor <- color
let set_background color = Console.BackgroundColor <- color
let moveto x y = Console.SetCursorPosition(x, y)

let fill_rect x y w h = 
    moveto x y
    Console.Write(Array.create w ' ')

let draw_string (str : string) = Console.Write(str)
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
        
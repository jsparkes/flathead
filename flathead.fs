module Flathead

open Type
open System.IO

let defaultFiles = [ "MiniZork.Z3"; "../../MiniZork.Z3" ]

[<EntryPoint>]
let main args = 
    let name =
        if args.Length > 0 then
            args.[0]
        else
            List.filter File.Exists defaultFiles |> List.head

    let story = Story.load name
    let screen = Screen.make (Character_height 50) (Character_width 80)
    let interpreter = Interpreter.make story screen
    let debugger = Debugger.make interpreter
    Debugger.run debugger
    0

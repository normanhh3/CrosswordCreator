module Program

open CrosswordCreator
open FSharp.Data
open System.IO

type PuzzleJson = JsonProvider<""" [{ "Word": "California", "Hint": "Biggest west coast state"}] """>
    
let getInputFromJson (file:string) :InputWord list option =    
    if not(File.Exists file) then
        None
    else
        let jsonS = File.ReadAllText file
        Some(
            (PuzzleJson.Parse jsonS)
            |> Array.map (fun (x) -> {Word = x.Word; Hint = x.Hint})
            |> Array.toList)

let (+/) path1 path2 = Path.Combine(path1, path2)

let getBinDir =
    Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)

[<EntryPoint>]
let main argv =
    (*
    let b = (Array2D.create 16 16 ' ')

    let (a, o) = getBoardBounds b

    printfn "Beg: %d and End: %d" a o

    printfn "(3,4) is inbounds (16,16): %b" (inbounds (3,4) b )
    printfn "(0,0) is inbounds (16,16): %b" (inbounds (0,0) b )
    printfn "(16,16) is inbounds (16,16): %b" (inbounds (16,16) b )
    *)

    //let inputs = Some([
    //    {Word = "Word One"; Hint = "Word One"};
    //    {Word = "Word Two"; Hint = "Word Two"};
    //])

    let filePath = getBinDir +/ "../../../../puzzles/Puzzle.json"
    let inputs = getInputFromJson filePath

    match inputs with
    | Some(puzzle) -> 
        let p = createPuzzle puzzle
        printfn "%s" (puzzleToString p) 
        0
    | None -> 
        printfn "Input file: %s does not exist!" filePath
        1
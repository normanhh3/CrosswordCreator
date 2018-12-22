module Program

open CrosswordCreator
open FSharp.Data
open System.IO

type PuzzleJson = JsonProvider<""" [{ "Word": "California", "Hint": "Biggest west coast state"}] """>
    
let getInputFromJsonS (json:string) :InputWord list =
    let jsonS = json
    (PuzzleJson.Parse jsonS)
            |> Array.map (fun (x) -> {Word = x.Word; Hint = x.Hint})
            |> Array.toList

let getInputFromJson (file:string) :InputWord list option =    
    if not(File.Exists file) then
        None
    else
        let jsonS = File.ReadAllText file
        Some(getInputFromJsonS jsonS)

let (+/) path1 path2 = Path.Combine(path1, path2)

let getBinDir =
    Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)

// TODO: Add a restriction to generation of the board that ensures letters placed 
// alongside of each other also make words in the list or they don't apply 

// TODO: Make case-sensitive matching an optional parameter

// TODO: Add post-processing step to shrink the 2d board down to minimum

[<EntryPoint>]
let main argv =    

    let filePath = getBinDir +/ "../../../../puzzles/Puzzle.json"
    let inputs = getInputFromJson filePath

    match inputs with
    | Some(wordList) -> 
        let basePuzzle = createEmptyPuzzle wordList
        let finalPuzzle = createPuzzles wordList basePuzzle |> Seq.tryHead
        match finalPuzzle with
        | None ->
            printfn "Oops! Unable to create a crossword puzzle with the given inputs!"
            2
        | Some(puzzle) ->
            printfn "%s" (puzzleToString puzzle) 
            0
    | None -> 
        printfn "Input file: %s does not exist!" filePath
        1
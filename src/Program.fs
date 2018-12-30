module Program

open CrosswordCreator
open FSharp.Data
open System.IO

type PuzzleJson = JsonProvider<""" [{ "Word": "California", "Hint": "Biggest west coast state"}] """>
    
let getInputFromJsonS (json:string) :InputWords =
    let jsonS = json
    (PuzzleJson.Parse jsonS)
        |> Array.map (fun (x) -> {Word = x.Word; Hint = x.Hint})
        |> Array.toList

let getInputFromJson (file:string) :InputWords option =    
    if not(File.Exists file) then
        printfn "Oops! Couldn't find the input file you referenced! %s" file
        None
    else
        let jsonS = File.ReadAllText file
        try
            Some(getInputFromJsonS jsonS)
        with
        | ex ->
            printfn "Oops! Couldn't load the JSON in %s because of %s" file ex.Message
            None

let (+/) path1 path2 = Path.Combine(path1, path2)

let getBinDir =
    Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)

// TODO: Add a restriction to generation of the board that ensures letters placed 
// alongside of each other also make words in the list or they don't apply 

// TODO: Make case-sensitive matching an optional parameter

// TODO: Add post-processing step to shrink the 2d board down to minimum

[<EntryPoint>]
let main argv =    

    let filePath = getBinDir +/ "../../../../puzzles/FamilyTree.json"
    let inputs = getInputFromJson filePath

    match inputs with
    | Some(wordList) -> 
        let basePuzzle = createEmptyPuzzle wordList
        let finalPuzzle = createPuzzles wordList basePuzzle |> Seq.skip 1 |> Seq.tryHead
        match finalPuzzle with
        | None ->
            printfn "Oops! Unable to create a crossword puzzle with the given inputs!"
            2
        | Some(puzzle) ->
            printfn "%s" (puzzleToHtml puzzle "Harebottle / McSherry Family Tree") 
            0
    | None -> 
        1
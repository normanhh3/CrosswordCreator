module Program

open Argu
open CrosswordCreator
open FSharp.Data
open System.IO
open System.Linq

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

[<EntryPoint>]
let main argv =    

    let filePath = getBinDir +/ "../../../../puzzles/FamilyTree.json"
    let inputs = getInputFromJson filePath

    match inputs with
    | Some(wordList) -> 

        let invalidWords = 
            wordList 
            |> Seq.where (fun w -> System.String.IsNullOrEmpty(w.Word) || System.String.IsNullOrEmpty(w.Hint))
            |> Seq.toList

        if not(List.isEmpty invalidWords) then
            printfn "Oops! Unable to create a crossword puzzle with the given word list!"
            printfn "Reason: Either a word or a hint was invalid!"
            3
        else
            let basePuzzle = createEmptyPuzzle wordList
            let finalPuzzle = createPuzzles wordList basePuzzle |> Seq.tryHead
            //|> Seq.skip 1 
            

            match finalPuzzle with
            | None ->
                printfn "Oops! Unable to create a crossword puzzle with the given inputs!"
                2
            | Some(puzzle) ->
                printfn "%s" 
                    //(puzzleToString puzzle)
                    (puzzleToHtml puzzle "Harebottle / McSherry Family Tree") 
                0
    | None -> 
        1
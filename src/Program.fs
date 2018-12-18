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

// TODO: Add a restriction to generation of the board that ensures letters placed 
// alongside of each other also make words in the list or they don't apply 

// TODO: Make case-sensitive matching an optional parameter

// TODO: Add post-processing step to shrink the 2d board down to minimum

[<EntryPoint>]
let main argv =    

    //let inputs = Some([
    //    {Word = "Word One"; Hint = "Word One"};
    //    {Word = "Word Two"; Hint = "Word Two"};
    //])

    let filePath = getBinDir +/ "../../../../puzzles/Sentence.json"
    let inputs = getInputFromJson filePath

    match inputs with
    | Some(wordlist) -> 
        let p = createPuzzle wordlist
        printfn "%s" (puzzleToString p) 
        0
    | None -> 
        printfn "Input file: %s does not exist!" filePath
        1
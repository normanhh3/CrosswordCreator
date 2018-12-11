module Program

open CrosswordCreator
        
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

    let inputs = [
        {Word = "Word One"; Hint = "Word One"};
        {Word = "Word Two"; Hint = "Word Two"};
    ]

    let p = createPuzzle inputs

    printfn "%s" (puzzleToString p) 
    
    0 // return an integer exit code
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
        {Word = "Harebottle"; Hint = "Family name"}; 
        {Word = "Norman III"; Hint = "Daddy"}; 
        (*
        {Word = "Angela"; Hint = "Mommy"}; 
        {Word = "Norman IV"; Hint = "Oldest"}; 
        {Word = "Savannah"; Hint = "Daughter"}; 
        {Word = "Declan"; Hint = "Third"}; 
        {Word = "Malcolm"; Hint = "NOT 3 but J"}; 
        {Word = "Lachlan"; Hint = "Omega"};
        {Word = "Harold"; Hint = "Middle of 2 but not she"};
        {Word = "Diane"; Hint = "Middle of 2 but not he"};
        {Word = "Richard"; Hint = "Middle of he and not she"};
        {Word = "Julian"; Hint = "1st Middle of he who is 3 of he"};
        {Word = "Blakely"; Hint = "2nd Middle of he who is 3 of he"};
        {Word = "Troy"; Hint = "Nickname of he not she"};
        {Word = "Bina"; Hint = "Nickname of she not he"};
        *)
        ]

    let p = createPuzzle inputs

    printfn "%s" (puzzleToString p) 
    
    0 // return an integer exit code
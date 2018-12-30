module Tests

open System
open System.Linq
open System.Collections.Generic
open Xunit
open CrosswordCreator

open TestHelpers
open Microsoft.VisualStudio.TestPlatform.TestHost

// Note: call waitForDebugger from TestHelpers.fs if you want to step into debugging a test

[<Fact>]
let ``between is inclusive`` () =
    Assert.True(between 9 10 9)
    Assert.False(between 0 10 -1)
    Assert.False(between 0 10 11)

let getEmptyTestBoard size =
    Array2D.create size size EmptyChar

[<Fact>]
let ``getBoardBounds returns 2d square array starting and ending indices`` () =
    let arrayDim = 15
    let b1 = getEmptyTestBoard arrayDim
    let b,e = getBoardBounds b1

    Assert.True (b = 0 && e = arrayDim - 1)

[<Fact>]
let ``inbounds ensures that coordinates are within the board`` () =
    let arrayDim = 15
    let b1 = getEmptyTestBoard arrayDim

    Assert.True (inbounds (0,0) b1)
    Assert.False (inbounds (-1,-1) b1)
    Assert.True (inbounds (14,14) b1)
    Assert.False (inbounds (15,15) b1)


[<Fact>]
let ``isWordPosEmpty returns true for a board with no words`` () =
    let b1 = getEmptyTestBoard 5
    let isEmpty = isWordPosEmpty (0,0) "Hello" Horizontal b1
    Assert.True isEmpty

 
[<Fact>]
let ``isWordPosEmpty returns false for a board with a word laid along the top`` () =
    let b1 = getEmptyTestBoard 5
    b1.[0,0] <- 'H'
    b1.[0,1] <- 'e'
    b1.[0,2] <- 'l'
    b1.[0,3] <- 'l'
    b1.[0,4] <- 'o'
    let isEmpty = isWordPosEmpty (0,0) "World" Horizontal b1
    Assert.False isEmpty


[<Fact>]
let ``isWordPosEmpty returns false for a board with a word laid along the bottom`` () =
    let b1 = getEmptyTestBoard 5
    b1.[4,0] <- 'H'
    b1.[4,1] <- 'e'
    b1.[4,2] <- 'l'
    b1.[4,3] <- 'l'
    b1.[4,4] <- 'o'
    let isEmpty = isWordPosEmpty (4,0) "World" Horizontal b1
    Assert.False isEmpty
    
[<Fact>]
let ``isWordPosEmpty returns false for a board with a word laid along the left`` () =
    let b1 = getEmptyTestBoard 5
    b1.[0,0] <- 'H'
    b1.[1,0] <- 'e'
    b1.[2,0] <- 'l'
    b1.[3,0] <- 'l'
    b1.[4,0] <- 'o'
    let isEmpty = isWordPosEmpty (0,0) "World" Horizontal b1
    Assert.False isEmpty

    
[<Fact>]
let ``isWordPosEmpty returns false for a board with a word laid along the right`` () =
    let b1 = layoutWord (0,4) Vertical "Hello" (getEmptyTestBoard 5)
    match b1 with
    | None -> Assert.True(false,"Word could not be laid out!")
    | Some(b) ->
        let r = isWordPosEmpty (0,4) "World" Vertical b
        Assert.False r

[<Fact>]
let ``isWordPosEmpty returns true for a board with a word laid along the right and a new word on the left`` () =
    let b1 = layoutWord (0,4) Vertical "Hello" (getEmptyTestBoard 5)
    match b1 with
    | None -> Assert.True(false,"Word could not be laid out!")
    | Some(b) ->
        let r = isWordPosEmpty (0,0) "World" Vertical b
        Assert.True r


[<Fact>]
let ``isWordPosEmpty returns true for a board with a word laid along the right and an overlapping word on the bottom`` () =
    let b1 = layoutWord (0,4) Vertical "Hello" (getEmptyTestBoard 5)
    match b1 with
    | None -> Assert.True(false,"Word could not be laid out!")
    | Some(b) ->
        //waitForDebugger
        let r = isWordPosEmpty (4,0) "Hello" Horizontal b
        Assert.True r

[<Fact>]
let ``findIntersectingPoints should not return an empty sequence when there are valid intersecting points`` () =
    let b1 = layoutWord (3,4) Vertical "Hello" (getEmptyTestBoard 10)
    match b1 with
    | None -> Assert.True(false,"Word could not be laid out!")
    | Some(b) ->
        //waitForDebugger
        let points = findIntersectingPoints b "World"
        Assert.False (Seq.isEmpty points)
        Assert.Equal(3, (Seq.length points))

// TODO: findIntersectingPoints needs to be more thoroughly tested to ensure that it is returning appropriate matches

open Program
open CrosswordCreator
open Xunit

[<Fact>]
let ``forAllColumnsInRowAreEmpty returns true for all rows and columns in an emmpty board`` () =
    let (board, wordcount, wordlist) = createEmptyPuzzle [{ Word="One"; Hint="1"}]
    let (b,e) = getBoardBounds board
    for rowOrCol in b .. e do
        Assert.True((forAllColumnsInRowAreEmpty rowOrCol board b e), sprintf "Row %d failed to pass!" rowOrCol)
        
        Assert.True((forAllRowsInColumnAreEmpty rowOrCol board b e), sprintf "Column %d failed to pass!" rowOrCol)

[<Fact>]
let ``forAllColumnsInRowAreEmpty returns true appropriate rows and columns in a board with one word`` () =
    //waitForDebugger
    let wl = [{ Word = "One"; Hint = "1" }]
    let puzzle = createEmptyPuzzle wl
    let (board, wordCount, wordList) = createPuzzles wl puzzle |> Seq.head
    Assert.Equal(3, Array2D.length1 board)
    Assert.Equal(3, Array2D.length2 board)

    // What word(s) were laid out on the board?
    Assert.Equal("One", (wordList |> Seq.head).Word)
    Assert.Equal("1", (wordList |> Seq.head).Hint)
    Assert.Equal((0,0), (wordList |> Seq.head).Coord)

    // Does the board contain the words that were laid out?
    Assert.Equal('O', board.[0,0])
    Assert.Equal('n', board.[0,1])
    Assert.Equal('e', board.[0,2])

    let (b, e) = getBoardBounds board

    Assert.False((forAllColumnsInRowAreEmpty 0 board b e), sprintf "Row %d failed to pass!" 0)
    Assert.True((forAllColumnsInRowAreEmpty 1 board b e), sprintf "Row %d failed to pass!" 1)
    Assert.True((forAllColumnsInRowAreEmpty 2 board b e), sprintf "Row %d failed to pass!" 2)

    for c in 0 .. 2 do
        Assert.False((forAllRowsInColumnAreEmpty c board b e), sprintf "Column %d failed to pass!" c)

[<Fact>]
let ``createPuzzles returns puzzles with smallest possible board size`` () =
    //waitForDebugger
    let wl = getInputFromJsonS """
        [
            {"Word": "Hello", "Hint": "A greeting"}, 
            {"Word": "Norman", "Hint": "A name"}, 
            {"Word": "What", "Hint": "A question about a noun"}, 
            {"Word": "Road", "Hint": "A transportation path"}, 
            {"Word": "Shall", "Hint": "Shall?"}, 
            {"Word": "We", "Hint": "Plural of people"}, 
            {"Word": "Drive?", "Hint": "Mode of transport"}
        ]
    """
    let basePuzzle = createEmptyPuzzle wl
    let resultingPuzzles = createPuzzles wl basePuzzle |> Seq.toList
    match resultingPuzzles with
    | [] -> Assert.True(false, "Whoops! No puzzles generated had all of the elements laid out!")
    | firstPuzzle :: _ -> 
        printfn "Wahoo! Found at least 1 puzzle that was valid (and %d more)!" resultingPuzzles.Length
        
        let (board, wc, wordList) = firstPuzzle
        
        let (b,e) = getBoardBounds board
        Assert.Equal(8, Array2D.length1 board)
        Assert.Equal(8, Array2D.length2 board)

        let hasLetterD = 
            seq {
                for r in b .. e do
                    for c in b .. e do
                        yield board.[r,c]
            } 
            |> Seq.distinct
            |> Seq.toList
            |> Seq.contains 'D'
        
        Assert.True(hasLetterD, "Whoops!  We couldn't find the letter D in 'Road' and 'Drive?'! That's bad news!")
        Assert.True(resultingPuzzles.Length > 1)
    |> ignore

// NOTE: The following two method implementations exist because I can't polymorphically access the Word field
// of two different record types in F#
// See: https://fslang.uservoice.com/forums/245727-f-language/suggestions/9633858-structural-extensible-records-like-elm-concrete


let getUniqueCharsFromInputWords (wl:InputWords) =
    new String(
        wl
        |> Seq.map (fun w -> w.Word.ToLowerInvariant()) 
        |> Seq.collect (fun ltrs -> ltrs.ToCharArray())
        |> Seq.sort
        |> Seq.toArray)

let getUniqueCharsFromPuzzleWords (pw:PuzzleWords) =
    new String(
        pw
        |> Seq.map (fun w -> w.Word.ToLowerInvariant()) 
        |> Seq.collect (fun ltrs -> ltrs.ToCharArray())
        |> Seq.sort
        |> Seq.toArray)
       

[<Fact>]
let ``createPuzzles returns only puzzles with all elements laid out`` () =
    //waitForDebugger
    let wl = getInputFromJsonS """
        [
            {"Word": "Hello", "Hint": "A greeting"}, 
            {"Word": "Norman", "Hint": "A name"}, 
            {"Word": "What", "Hint": "A question about a noun"}, 
            {"Word": "Road", "Hint": "A transportation path"}, 
            {"Word": "Shall", "Hint": "Shall?"}, 
            {"Word": "We", "Hint": "Plural of people"}, 
            {"Word": "Drive?", "Hint": "Mode of transport"}
        ]
    """
    let basePuzzle = createEmptyPuzzle wl

    let uniqueLettersInWL = getUniqueCharsFromInputWords wl
    let wlS = new String(uniqueLettersInWL.ToArray())

    let resultingPuzzles = createPuzzles wl basePuzzle |> Seq.toList
    match resultingPuzzles with
    | [] -> Assert.True(false, "Whoops! No puzzles generated had all of the elements laid out!")
    | _ -> 
        printfn "Wahoo! Found at least 1 puzzle that was valid (and %d more)!" resultingPuzzles.Length
        for p in resultingPuzzles do
            let (_, _, wlP) = p
            let uniqueLettersInPuzzle = getUniqueCharsFromPuzzleWords wlP
            let r = uniqueLettersInWL = uniqueLettersInPuzzle
            if not r then
                printfn "The following puzzle doesn't appear to have all of the input words laid out properly!"
                
                printfn "Input letters:  '%s'" uniqueLettersInWL
                printfn "Puzzle letters: '%s'" uniqueLettersInPuzzle
                printfn "%s" (puzzleToString p)
                Assert.True(false)
            
        Assert.True(resultingPuzzles.Length > 1)


[<Fact>]
let ``shrinkPuzzleToSmallest results in a smaller board`` () =
    //waitForDebugger
    let wl = [ {Word="We"; Hint="We";}; {Word="Will"; Hint="Will";}; {Word="Look!"; Hint="Look!";} ]
    let basePuzzle = createEmptyPuzzle wl

    // Add first word to the board
    let puzzleWithWe = 
        addWordToPuzzle basePuzzle (wl |> List.head) 
        |> Seq.head
    
    // Verify placement of first word on the board
    match puzzleWithWe with
    | (brd, wc, wdL) ->
        Assert.Equal(1, wc)
        Assert.Equal(1, wdL.Length)

        let (b,e) = getBoardBounds brd
        Assert.Equal(0, b)
        Assert.Equal(14, e)

        let word = wdL |> List.head
        Assert.Equal("We", word.Word)
        Assert.Equal("We", word.Hint)
        Assert.Equal((7,6), word.Coord)
        Assert.Equal(Horizontal, word.Dir)

        Assert.Equal('W', brd.[7,6])
        Assert.Equal('e', brd.[7,7])

    
    // Add second word on the board
    let puzzleWithWill = 
        addWordToPuzzle puzzleWithWe (wl |> (List.skip 1) |> List.head)
        |> Seq.where (fun (_,wc,_) -> wc = 2) 
        |> Seq.head
    
    // Verify placement of second word on the board
    match puzzleWithWill with
        | (brd, wc, wdL) ->
            Assert.Equal(2, wc)
            Assert.Equal(2, wdL.Length)

            let (b,e) = getBoardBounds brd
            Assert.Equal(0, b)
            Assert.Equal(14, e)

            let word = wdL |> List.head
            Assert.Equal("Will", word.Word)
            Assert.Equal("Will", word.Hint)
            Assert.Equal((7,6), word.Coord)
            Assert.Equal(Vertical, word.Dir)

            Assert.Equal('W', brd.[7,6])
            Assert.Equal('i', brd.[8,6])
            Assert.Equal('l', brd.[9,6])
            Assert.Equal('l', brd.[10,6])
        |> ignore

    // Add third word to the board
    let look = (wl |> (List.skip 2) |> List.head)
    let puzzleWithLook = 
        addWordToPuzzle puzzleWithWill look
        |> Seq.where (fun (_,wc,_) -> wc = 3)
        |> Seq.tryHead

    // Verify placement of third word on the board
    match puzzleWithLook with
        | None ->
            Assert.True(false, sprintf "Failed to add the word %s to the base puzzle\r\n %s" look.Word (puzzleToString puzzleWithWill))
        | Some(brd, wc, wdL) ->
            Assert.Equal(3, wc)
            Assert.Equal(3, wdL.Length)

            let (b,e) = getBoardBounds brd
            Assert.Equal(0, b)
            Assert.Equal(14, e)

            let word = wdL |> List.head
            Assert.Equal("Look!", word.Word)
            Assert.Equal("Look!", word.Hint)
            Assert.Equal((9,6), word.Coord)
            Assert.Equal(Horizontal, word.Dir)

            Assert.Equal('L', brd.[9,6])
            Assert.Equal('o', brd.[9,7])
            Assert.Equal('o', brd.[9,8])
            Assert.Equal('k', brd.[9,9])
            Assert.Equal('!', brd.[9,10])
        |> ignore
    
    (*
    let smallPuzzle = shrinkPuzzleToSmallest inputPuzzle
    match smallPuzzle with
    | (smallBoard, wordCount, wordList) ->

        Assert.Equal(2, (Array2D.length1 smallBoard))
        Assert.Equal(2, (Array2D.length2 smallBoard))

        Assert.Equal('W', smallBoard.[0,0])
        Assert.Equal('e', smallBoard.[0,1])

        let word = wordList |> Seq.head

        Assert.Equal("We", word.Word)
        Assert.Equal(Horizontal, word.Dir)
        Assert.Equal((0,0), word.Coord)
        
        let (minB, maxB) = getBoardBounds smallBoard
        Assert.Equal(0, minB)
        Assert.Equal(1, maxB)
    |> ignore
    *)

[<Fact>]
let ``shrinkPuzzleToSmallest with more complex board results in a smaller board`` () =
    //waitForDebugger
    let wl = getInputFromJsonS """
        [
            {"Word": "Hello", "Hint": "A greeting"}, 
            {"Word": "Norman", "Hint": "A name"}, 
            {"Word": "What", "Hint": "A question about a noun"}, 
            {"Word": "Road", "Hint": "A transportation path"}, 
            {"Word": "Shall", "Hint": "Shall?"}, 
            {"Word": "We", "Hint": "Plural of people"}, 
            {"Word": "Drive?", "Hint": "Mode of transport"}
        ]
    """
    let basePuzzle = createEmptyPuzzle wl
    let (baseBoard, _, _) = basePuzzle
    let (_, baseE) = getBoardBounds baseBoard
    let resultingPuzzles = createPuzzles wl basePuzzle |> Seq.toList
    match resultingPuzzles with
    | [] -> Assert.True(false, "Whoops! No puzzles generated had all of the elements laid out!")
    | _ -> 
        printfn "Wahoo! Found at least 1 puzzle that was valid (and %d more)!" resultingPuzzles.Length
        for (board,wordCount,wordList) in resultingPuzzles do
            //printfn "%s" (puzzleToString p)
            let (b,e) = getBoardBounds board
            Assert.True(e < baseE, "Whoops! Expected a smaller board!")
        Assert.True(resultingPuzzles.Length > 1)
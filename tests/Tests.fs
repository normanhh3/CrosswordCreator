module Tests

open System
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
    let resultingPuzzles = createPuzzles wl basePuzzle |> Seq.toList
    match resultingPuzzles with
    | [] -> Assert.True(false, "Whoops! No puzzles generated had all of the elements laid out!")
    | _ -> 
        printfn "Wahoo! Found at least 1 puzzle that was valid (and %d more)!" resultingPuzzles.Length
        for p in resultingPuzzles do
            printfn "%s" (puzzleToString p)
        Assert.True(resultingPuzzles.Length > 1)


[<Fact>]
let ``shrink board results in a smaller board`` () =
    //waitForDebugger
    let b1 = getEmptyTestBoard 5
    b1.[3,2] <- 'W'
    b1.[3,3] <- 'e'
    let inputPuzzle = (b1, 1, [ {Word="We"; Hint="We"; Coord=(3,2); Dir=Horizontal} ])

    let smallPuzzle = shrinkPuzzleToSmallest inputPuzzle
    match smallPuzzle with
    | (smallBoard, wordCount, wordList) ->

        Assert.Equal(2, (Array2D.length1 smallBoard))
        Assert.Equal(2, (Array2D.length2 smallBoard))

        let (minB, maxB) = getBoardBounds smallBoard
        Assert.Equal(0, minB)
        Assert.Equal(1, maxB)
    |> ignore

[<Fact>]
let ``reverse sequence expressions work`` () =
    Assert.Equal(3, seq {7 .. 10 .. 7} |> Seq.length)
    Assert.Equal(10, seq {10 .. 7} |> Seq.head)
    Assert.Equal(7, seq {10 .. 7} |> Seq.last)
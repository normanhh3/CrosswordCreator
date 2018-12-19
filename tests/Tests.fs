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
    Array2D.create size size ' '

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

open Program

[<Fact>]
let ``layout of a sentence of words should include all words`` () =
    //waitForDebugger
    let wl = getInputFromJsonS """
        [
            {"Word": "Hello", "Hint": "1"}, 
            {"Word": "Norman", "Hint": "2"}, 
            {"Word": "What", "Hint": "3"}, 
            {"Word": "Road", "Hint": "4"}, 
            {"Word": "Shall", "Hint": "5"}, 
            {"Word": "We", "Hint": "6"}, 
            {"Word": "Take?", "Hint": "7"}
        ]
    """
    match createPuzzle wl with
    | None -> Assert.True(false, "Failed to layout the puzzle as expected!")
    | Some(board, wordCount, words) -> 
        let puzzleWords = words |> Seq.map (fun r -> r.Word) |> Set.ofSeq
        let expectedWords = wl |> Seq.map (fun r -> r.Word) |> Set.ofSeq
        let diff = Set.difference expectedWords puzzleWords
        Assert.Empty diff

        Assert.Equal(7, wordCount)


(*
[<Fact>]
let ``shrink board results in a smaller board`` () =
    //waitForDebugger
    let b1 = getEmptyTestBoard 5
    b1.[3,3] <- 'X'

    let smallBoard = shrinkBoardToSmallest ' ' b1
    Assert.Equal(1,(Array2D.length1 smallBoard))
    Assert.Equal(1, (Array2D.length2 smallBoard))

    let (minB, maxB) = getBoardBounds smallBoard
    Assert.Equal(0, minB)
    Assert.Equal(0, maxB)
*)
module Tests

open System
open Xunit
open CrosswordCreator

open TestHelpers

[<Fact>]
let ``between is inclusive`` () =
    Assert.True(between 9 10 9)
    Assert.False(between 0 10 -1)
    Assert.False(between 0 10 11)

let getTestBoard size =
    Array2D.create size size ' '

[<Fact>]
let ``getBoardBounds returns 2d square array starting and ending indices`` () =
    let arrayDim = 15
    let b1 = getTestBoard arrayDim
    let b,e = getBoardBounds b1

    Assert.True (b = 0 && e = arrayDim - 1)

[<Fact>]
let ``inbounds ensures that coordinates are within the board`` () =
    let arrayDim = 15
    let b1 = getTestBoard arrayDim

    Assert.True (inbounds (0,0) b1)
    Assert.False (inbounds (-1,-1) b1)
    Assert.True (inbounds (14,14) b1)
    Assert.False (inbounds (15,15) b1)


[<Fact>]
let ``isWordPosEmpty returns true for a board with no words`` () =
    let b1 = getTestBoard 5
    let isEmpty = isWordPosEmpty (0,0) "Hello" Horizontal b1
    Assert.True isEmpty

 
[<Fact>]
let ``isWordPosEmpty returns false for a board with a word already laid out`` () =
    //waitForDebugger
    let b1 = getTestBoard 5
    b1.[0,0] <- 'H'
    b1.[0,1] <- 'e'
    b1.[0,2] <- 'l'
    b1.[0,3] <- 'l'
    b1.[0,4] <- 'o'
    let isEmpty = isWordPosEmpty (0,0) "World" Horizontal b1
    Assert.False isEmpty

[<Fact>]
let ``shrink board results in a smaller board`` () =
    let b1 = getTestBoard 5
    b1.[3,3] <- 'X'

    let smallBoard = shrinkBoardToSmallest ' ' b1
    Assert.True ((Array2D.length1 smallBoard) = 1)
    Assert.True ((Array2D.length2 smallBoard) = 1)

    let (minB, maxB) = getBoardBounds smallBoard
    Assert.True (minB = 0 && maxB = 0)

module Tests

open System
open Xunit
open CrosswordCreator

[<Fact>]
let ``between is inclusive`` () =
    Assert.True(between 9 10 9)
    Assert.False(between 0 10 -1)
    Assert.False(between 0 10 11)

let ``getBoardBounds returns 2d square array starting and ending indices`` =
    let arrayDim = 15
    let b1 = Array2D.create arrayDim arrayDim ' '
    let b,e = getBoardBounds b1

    Assert.True (b = 0 && e = arrayDim - 1)
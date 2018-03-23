module AsyncResultTests

open System
open Xunit
open FsUnit

open AsyncResult
open Request
open Request.Request
open FsUnit.Xunit


[<Fact>]
let ``AsyncResult builder async`` () =
  asyncResult {
    let! a = AsyncResult.ofResult(Ok 3)
    let! b = AsyncResult.ofResult(Ok 5)
    a + b |> should equal 8
  } : AsyncResult<unit, unit>

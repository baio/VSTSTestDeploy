module ReaderAsyncResultTests

open System
open Xunit
open FsUnit

open AsyncResult
open ReaderAsyncResult
open FsUnit.Xunit


[<Fact>]
let ``ReaderAsyncResult builder async`` () =

    (
    readerAsyncResult {
        let! a = ReaderAsyncResult.ofResult(Ok 3)
        let! b = ReaderAsyncResult.ofResult(Ok 5)
        a + b |> should equal 8
    } |> ReaderAsyncResult.run 0
    ) : AsyncResult<unit, Object>

[<Fact>]
let ``ReaderAsyncResult builder async with env`` () =

    (
    readerAsyncResult {
        let! a = ReaderAsyncResult(fun env -> env + 3 |> AsyncResult.rtn )
        let! b = ReaderAsyncResult(fun env -> env + 5 |> AsyncResult.rtn )
        a + b |> should equal 14
    } |> ReaderAsyncResult.run 3
    ) : AsyncResult<unit, Object>


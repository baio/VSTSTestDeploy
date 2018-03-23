module HttpRequestTests

open System
open Xunit
open FsUnit

open Request
open Request.HttpRequest
open FsUnit.Xunit
open ReaderAsyncResult
open AsyncResult

type Payload = {
    Lol : bool
}

type FormPayload = {
    form : Payload
}

type FormPayloadWrong = {
    form1 : Payload
}

let checkNoErrors res = res |> mapError (fun err ->
    err |> should be Empty
    err
)

[<Fact>]
let ``Post request must mork`` () =
    asyncResult {
        let! res = post<FormPayload> "https://httpbin.org/post" (FormPayload [("Lol", "true")])
        let expected = { form = { Lol = true } }
        res |> should equal expected
    }

[<Fact>]
let ``Post (with collectM) request must mork`` () =

    let reqList =
        [
            "https://httpbin.org/put?tok=1", FormPayload [("Lol", "true")], PUT
            "https://httpbin.org/post?tok=2", FormPayload [("Lol", "false")], POST
        ]
        |> List.map(fun (x, y, z) ->
            Request.request<FormPayload> { url = x; payload = y; headers = []; httpMethod = z }
            |> ReaderAsyncResult.delay 100
        )
        |> ReaderAsyncResult.collectM

    let expected = [
        { form = { Lol = true } }
        { form = { Lol = false } }
    ]

    ReaderAsyncResult.readerAsyncResult {
        let! res = reqList
        res |> should equal expected
        return res
    }
    |> ReaderAsyncResult.run { request = request }
    |> checkNoErrors

[<Fact>]
let ``Chunk request must work`` () =

    let reqList =
        [
            "https://httpbin.org/put?tok=1", FormPayload [("Lol", "true")], PUT
            "https://httpbin.org/post?tok=2", FormPayload [("Lol", "false")], POST
            "https://httpbin.org/put?tok=1", FormPayload [("Lol", "false")], PUT
            "https://httpbin.org/post?tok=2", FormPayload [("Lol", "true")], POST
        ]
        |> List.map(fun (x, y, z) ->
            Request.request<FormPayload> { url = x; payload = y; headers = []; httpMethod = z }
        )
        |> ReaderAsyncResult.chunk 2 500

    let expected = [
        { form = { Lol = true } }
        { form = { Lol = false } }
        { form = { Lol = false } }
        { form = { Lol = true } }
    ]

    ReaderAsyncResult.readerAsyncResult {
        let! res = reqList
        res |> should equal expected
        return res
    }
    |> ReaderAsyncResult.run { request = request }
    |> checkNoErrors


[<Fact>]
let ``Post request with wrong url must fail with NameResolutionFailure`` () =
    asyncResult {
        return! post<FormPayload> "https://httpbin1.org/post1" (FormPayload [("Lol", "true")])
        |> mapError (function
            | WebError ex -> ex.Status |> should equal System.Net.WebExceptionStatus.NameResolutionFailure
            | _ -> true |> should equal false
        )
    }

[<Fact>]
let ``Post request with wrong payload type must return null for a wrong field`` () =
    asyncResult {
        let! result = post<FormPayloadWrong> "https://httpbin.org/post" (FormPayload [("Lol", "true")])
        result.form1 |> should equal null
    }

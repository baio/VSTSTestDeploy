module ReqestTests

open System
open Xunit
open FsUnit

open ReaderAsyncResult
open Request
open Request.Request
open FsUnit.Xunit

type Payload = {
    Lol : bool
}

type FormPayload = {
    form : Payload
}

type FormPayloadWrong = {
    form1 : Payload
}

[<Fact>]
let ``Post request must work`` () =

    let request req  =
        if req.httpMethod = POST && req.url = "https://httpbin.org/post" then
           AsyncResult.rtn "{ form : { Lol : true } }"
        else
           AsyncResult.rtn ""

    let res = post<FormPayload> "https://httpbin.org/post" (FormPayload [("Lol", "true")]) |> run { request = request } |> Async.RunSynchronously

    let expected = { form = { Lol = true } }
    match res with
    | Ok x -> x |> should equal expected
    | Error err -> err |> should be Empty


[<Fact>]
let ``Post request (traverse) must work`` () =

    let request req  =
        if req.httpMethod = POST && req.url = "https://httpbin.org/post" then
            match req.payload with
            | FormPayload [(x, y)] ->
                AsyncResult.rtn(sprintf "{ %s : %s }" x y)
            | _ -> 
                failwith "impossible"
        else
           AsyncResult.rtn ""

    let reqList = 
        [
            "https://httpbin.org/post", (FormPayload [("Lol", "true")])
            "https://httpbin.org/post", (FormPayload [("Lol", "false")])
        ] 
        |> List.map(fun (x, y) -> post<Payload> x y)
        |> collectA

    let expected = [
        { Lol = true }
        { Lol = false }
    ]

    readerAsyncResult {
        let! result = reqList
        result |> should equal expected
        return result
    } |> run { request = request }

[<Fact>]
let ``Post request (traverse with error) must work`` () =

    let request req  =
        if req.httpMethod = POST && req.url = "https://httpbin.org/post" then
            match req.payload with
            | FormPayload [(x, y)] ->
                match y with
                | "true" -> AsyncResult.rtn(sprintf "{ %s : %s }" x y)
                | _  -> "Err" :> obj |> HttpError.UnknowError |> Error |> AsyncResult.ofResult
            | _ -> 
                failwith "impossible"
        else
           AsyncResult.rtn ""

    let reqList = 
        [
            "https://httpbin.org/post", (FormPayload [("Lol", "true")])
            "https://httpbin.org/post", (FormPayload [("Lol", "false")])
        ] 
        |> List.map(fun (x, y) -> post<Payload> x y)
        |> collectA

    let expected = 
        [
            Ok { Lol = true }
            Error ("Err" :> obj |> HttpError.UnknowError )
        ]

    readerAsyncResult {
        let! result = reqList
        result |> should be Empty
        return result
    } 
    |> run { request = request }
    |> AsyncResult.mapError(fun err ->
        err |> should equal expected
    )


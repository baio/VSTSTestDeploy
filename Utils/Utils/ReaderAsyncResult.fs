module ReaderAsyncResult

open AsyncResult
open System

type ReaderAsyncResult<'Env, 'T, 'TError> = ReaderAsyncResult of ('Env -> AsyncResult<'T, 'TError>)

let rtn x = ReaderAsyncResult (fun _ -> AsyncResult.rtn x)

let run env a =
    match a with
    | ReaderAsyncResult r -> r env

let map (f : 'a -> 'b) (a : ReaderAsyncResult<'e, 'a, 'c>) : ReaderAsyncResult<'e, 'b, 'c> =
    match a with
    | ReaderAsyncResult r ->
        ReaderAsyncResult (fun env -> AsyncResult.map f (r env))

let mapError f a =
    match a with
    | ReaderAsyncResult r ->
        ReaderAsyncResult (fun env -> AsyncResult.mapError f (r env))

let bind (f : 'a -> ReaderAsyncResult<'e, 'b, 'c>) (a : ReaderAsyncResult<'e, 'a, 'c>) : ReaderAsyncResult<'e, 'b, 'c> =
    match a with
    | ReaderAsyncResult r ->
        ReaderAsyncResult (fun env -> AsyncResult.bind (f >> run env) (r env))

let bindError f a =
    match a with
    | ReaderAsyncResult r ->
        ReaderAsyncResult (fun env -> AsyncResult.bindError (f >> run env) (r env))

let inline (>>=) x f = bind f x
let inline (!==) x f = bindError f x
let inline (<!>) f x = map f x


type Collect<'Env, 'T, 'TError> = ReaderAsyncResult<'Env, 'T, 'TError> list -> ReaderAsyncResult<'Env, 'T list, Result<'T, 'TError> list>

let collectA: Collect<_, _, _> = fun list ->
    ReaderAsyncResult (fun env ->
        list |> List.map (run env) |> Async.Parallel
            |> Async.map(Array.toList >> Result.collectA)
    )

let collectM: Collect<_, _, _> = fun list ->
    ReaderAsyncResult (fun env ->
        async {
            let results = new System.Collections.ArrayList()
            for x in list do
                let! r = run env x
                results.Add r |> ignore

            return results.ToArray() |> List.ofSeq |> List.map (fun x -> x :?> Result<_, _>) |> Result.collectA
        }
    )

let ofAsyncResult<'T, 'TError> (r : AsyncResult<'T, 'TError>) = ReaderAsyncResult (fun _ -> r)

let ofResult<'Env, 'T, 'TError> (r : Result<'T, 'TError>) = ReaderAsyncResult (fun (_: 'Env) -> AsyncResult.ofResult r)

// unique name across solution (in case another module hides ofResult)
let rofResult = ofResult

let ofErrorMap f = f >> Error >> ofResult

type OfErrorAsyncResult<'E, 'T, 'TError, 'TError1> =
    ('TError -> 'TError1) -> AsyncResult<'T, 'TError> -> ReaderAsyncResult<'E, 'T, 'TError1>
let ofErrorAsyncResult: OfErrorAsyncResult<_, _, _, _> = fun f err ->
    err |> (AsyncResult.mapError f) |> ofAsyncResult

type OfErrorResult<'E, 'T, 'TError, 'TError1> =
    ('TError -> 'TError1) -> Result<'T, 'TError> -> ReaderAsyncResult<'E, 'T, 'TError1>
let ofErrorResult: OfErrorResult<_, _, _, _> = fun f err ->
    err |> (Result.mapError f) |> ofResult

[<Measure>]
type ms

type Delay<'Env, 'T, 'TError> = int -> ReaderAsyncResult<'Env, 'T, 'TError> -> ReaderAsyncResult<'Env, 'T, 'TError>
let delay: Delay<_, _, _> = fun x r ->
    r >>= (fun y -> ReaderAsyncResult(fun _ ->
        async {
            do! Async.Sleep x
            return Ok y
        }
    ))

let private mapChunkError err = 
    List.fold (fun acc y -> 
        match y with
        | Ok x -> x |> List.map Ok |> List.append acc
        | Error x -> x |> List.append acc
        ) [] err

type Chunk<'Env, 'T, 'TError> = int -> int -> ReaderAsyncResult<'Env, 'T, 'TError> list -> ReaderAsyncResult<'Env, 'T list, Result<'T, 'TError> list>
let chunk: Chunk<_, _, _> = fun batchSize timeout ->
    List.chunkBySize batchSize
    >> List.map collectA
    >> List.map (delay timeout)
    >> collectM
    >> map List.concat
    >> mapError mapChunkError

type ReaderAsyncResultBuilder() =

    member this.Bind(m, f) = bind f m

    member this.Return(x) = rtn x

    member this.ReturnFrom(m) = m

    member this.Zero() = this.Return()

    member this.Combine (r1 : ReaderAsyncResult<_,_,_>, r2 : ReaderAsyncResult<_,_,_>)
        : ReaderAsyncResult<'Env, 'T, 'Error> =
        this.Bind (r1, (fun () -> r2))

    member this.Delay(generator: unit -> ReaderAsyncResult<_, _, _>): ReaderAsyncResult<_, _, _> =
        ReaderAsyncResult(fun env -> generator () |> run env)

    member this.TryWith(body, handler) =
        ReaderAsyncResult(fun env ->
            asyncResult {
                try
                    return! body() |> run env
                with
                | e ->
                    return! handler e |> run env
            }
        )

let readerAsyncResult = new ReaderAsyncResultBuilder()
module AsyncResult

// https://github.com/jack-pappas/ExtCore

open System

type AsyncResult<'T, 'TError> = Async<Result<'T, 'TError>>

let choise2result choise =
    match choise with
    | Choice1Of2 res -> Ok res
    | Choice2Of2 err -> Error err


let ofAsync<'T> a: AsyncResult<'T, exn> =
    async {
        let! choise = a |> Async.Catch
        return choise2result choise
    }

let ofTask<'T> = Async.AwaitTask >> ofAsync<'T>

let ofResult<'T, 'TError> (r : Result<'T, 'TError>) =
    async {
        return r
    }

type OfErrorResult<'T, 'TError, 'TError1> = 
    ('TError -> 'TError1) -> Result<'T, 'TError> -> AsyncResult<'T, 'TError1>
let ofErrorResult: OfErrorResult<_, _, _> = fun f err ->
    err |> (Result.mapError f) |> ofResult

let rtn x = x |> Ok |>async.Return

let map (f : 'a -> 'b) (a : AsyncResult<'a, 'c>) : AsyncResult<'b, 'c> =
    async {
        let! res = a 
        return res |> Result.map(f)
    }

let bind (f : 'a -> AsyncResult<'b, 'c>) (a : AsyncResult<'a, 'c>) : AsyncResult<'b, 'c> =
    async {
        let! res = a 
        return! 
            match res with
            | Ok x -> f x
            | Error err -> async { return Error err }
    }

let mapError f a =
    async {
        let! res = a 
        return res |> Result.mapError(f)
    }

let bindError f a =
    async {
        let! res = a 
        match res with
        | Ok x -> 
            return Ok x
        | Error err ->
            let! x = f err
            return x
    } 

let ofErrorMap f = f >> Error >> ofResult


[<Sealed>]
type AsyncResultBuilder() =

    member (*inline*) this.Bind (value : AsyncResult<'T, 'E>, binder : 'T -> AsyncResult<'U, 'E>)
        : AsyncResult<'U, 'E> =
        async {
        let! value' = value
        match value' with
        | Error error ->
            return Error error
        | Ok x ->
            return! binder x
        }


    member this.Return(x) = rtn x

    member this.ReturnFrom(m) = m

    member this.Zero() = this.Return()

    member this.Combine (r1, r2) : AsyncResult<'T, 'E> =
        async {
            let! r1' = r1
            match r1' with
            | Error error ->
                return Error error
            | Ok _ ->
                return! r2
        }

    member this.Delay generator = 
        async.Delay generator
       
    member inline __.TryWith (computation : AsyncResult<'T, 'Error>, catchHandler : exn -> AsyncResult<'T, 'Error>)
        : AsyncResult<'T, 'Error> =
        async.TryWith(computation, catchHandler)

let asyncResult = new AsyncResultBuilder()

// https://fsharpforfunandprofit.com/posts/elevated-world-5/

let apply fAsync xAsync = asyncResult {
        
    // start the two asyncs in parallel
    let! fChild = Async.StartChild fAsync |> ofAsync
    let! xChild = Async.StartChild xAsync |> ofAsync

    // wait for the results
    let! f = fChild
    let! x = xChild 

    // apply the function to the results
    return f x 
}

let inline (>>=) x f = bind f x
let inline (!==) x f = bindError f x
let inline (<!>) f x = map f x
let inline (>!<) x f = map f x
let inline (<*>) f x = apply f x


// https://fsharpforfunandprofit.com/posts/elevated-world-4/#traverse
let traverseA f list =

    // define the applicative functions

    // define a "cons" function
    let cons head tail = head :: tail

    // right fold over the list
    let initState = rtn []
    let folder head tail = 
        rtn cons <*> (f head) <*> tail

    List.foldBack folder list initState 

let sequenceA x = traverseA id x
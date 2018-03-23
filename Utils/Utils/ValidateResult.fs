namespace Utils

module ValidateResult =
 
    type FieldError = {
        field: string option
        code: string
        errorDescription: string
    }

    type ValidateResultError =
        | ValidateResultError of FieldError list

    type ValidateResult<'T> = Result<'T, ValidateResultError>

    let rtn x: ValidateResult<'a> = x |> Ok

    let ofResult<'a> (field: string) (r: Result<'a, string>) =
        match r with
        | Ok v -> rtn v
        | Error err -> 
            [{ field = Some field; code = err; errorDescription = err  }] |> ValidateResultError |> Error

    let ofResult2<'a> (r: Result<'a, string>) =
        match r with
        | Ok v -> rtn v
        | Error err -> 
            [{ field = None; code = err; errorDescription = err  }] |> ValidateResultError |> Error

    let map (f: 'a -> 'b) (m: ValidateResult<'a>): ValidateResult<'b> =
        match m with
        | Ok v ->
          (f v) |> rtn
        | Error e -> e |> Error

    let bind (f: 'a -> ValidateResult<'b>) (m: ValidateResult<'a>): ValidateResult<'b> =
        match m with
        | Ok x -> f x
        | Error errs -> Error errs

    let apply (fm: ValidateResult<'a -> 'b>) (m: ValidateResult<'a>) =
        match fm, m with
        | Ok f, Ok x ->
            Ok (f x)
        | Error errs, Ok _ ->
            Error errs
        | Ok _, Error errs ->
            Error errs
        | Error errs1, Error errs2 ->
            match errs1, errs2 with 
                | ValidateResultError x, ValidateResultError y ->
                    List.concat [x; y] |> ValidateResultError |> Error 
    
    let (<!>) = map
    let (<*>) = apply
    let inline (>>=) x f = bind f x


module Result

type CollectA<'T, 'Err> = Result<'T, 'Err> list -> Result<'T list, Result<'T, 'Err> list>
let collectA: CollectA<'T, 'Err> = fun list ->
    List.foldBack(fun r acc  -> 
        match r, acc with
        | Ok x, Ok y -> List.append [x] y |> Ok
        | Ok x, Error y -> List.append [Ok x] y |> Error
        | Error x, Ok y -> List.append [(Error x)] (y |> List.map Ok) |> Error
        | Error x, Error y -> List.append [(Error x)] y |> Error
    ) list (Ok [])

let bindError f r =
    match r with
    | Ok x -> Ok x
    | Error err -> f err

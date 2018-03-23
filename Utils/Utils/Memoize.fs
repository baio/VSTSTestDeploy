module Memoize


let memoize f = 
    let cache = System.Collections.Generic.Dictionary() 
    fun x -> 
        match cache.TryGetValue(x) with 
        | true, res -> 
            res 
        | _ -> 
            let res = f x 
            cache.Add(x,res) 
            res 

let memoize2 reset f = 
    let cache = System.Collections.Generic.Dictionary() 
    fun x -> 
        match reset with 
            | false ->
                match cache.TryGetValue(x) with 
                | true, res -> res 
                | _ -> 
                    let res = f x 
                    cache.Add(x,res) 
                    res 
            | true ->
                let res = f x 
                cache.[x] <- res
                res 

let inline memoizeAsync f =
    let dict = System.Collections.Concurrent.ConcurrentDictionary()
    fun x -> async {
        match dict.TryGetValue x with
        | true, result -> return result
        | false, _ ->
            let! result = f x
            dict.TryAdd(x, result) |> ignore
            return result
    }

let inline memoizeAsync2 f =
    let dict = System.Collections.Concurrent.ConcurrentDictionary()
    fun reset x -> async {
        match reset with
            | true ->
                match dict.TryGetValue x with
                | true, result -> return result
                | false, _ ->
                    let! result = f x
                    dict.TryAdd(x, result) |> ignore
                    return result
             | false ->
                    let! result = f x
                    dict.AddOrUpdate(x, 
                        addValue = result, 
                        updateValueFactory = System.Func<_,_,_>(fun _ _ -> result)
                    ) |> ignore
                    return result                
    }
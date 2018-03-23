module Async

let map f m = async {
    let! x = m
    return f x
}

let mapResult fOk fErr m = async {
    let! x = m
    return 
        match x with 
            | Ok r -> fOk r
            | Error err -> fErr err
}

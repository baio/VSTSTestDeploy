[<AutoOpen>]
module UtilsStuff

let isNull x = match x with null -> true | _ -> false

let asObj x = x :> obj

// TODO : Error: Object is not JSON !

type NotJson = NotJson of string

let jsonFromStream<'T> (stream: System.IO.Stream) =

    use reader = new System.IO.StreamReader(stream)

    let str = reader.ReadToEnd()

    let res = Newtonsoft.Json.JsonConvert.DeserializeObject<'T> str

    if isNull (res :> obj) then Error (NotJson str) else Ok res

let opt2null<'T1, 'T2 when 'T2: null> f (x:  'T1 option): 'T2 =
    match x with Some x -> f x | None -> null
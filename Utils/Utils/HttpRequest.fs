namespace Request

module HttpRequest = 

    open System.Net
    open AsyncResult
    open Request
    open FSharp.Data

    let mapError (e: exn) = 
        match e with 
        | :? WebException as ex ->            
            WebError ex
        | _ ->
            UnknowError e

    let request (req : Request) =

        let httpMethod = 
            match req.httpMethod with
            | POST -> "POST"
            | PATCH -> "PATCH"
            | PUT -> "PUT"
            | DELETE -> "DELETE"
            | GET -> "GET"
        
        let payload = 
            match req.payload with
            | FormPayload x -> Some(FormValues x)
            | JsonPayload x -> 
                x |> Newtonsoft.Json.JsonConvert.SerializeObject |> TextRequest |> Some
            | None -> Option.None 

        let headers = 
            match req.payload with
            | JsonPayload _ -> Seq.append req.headers ["content-type", "application/json"]
            | _ -> req.headers
        
        let httpAsync = 
            match payload with 
            | Some x -> Http.AsyncRequestString (req.url, headers = headers, body = x, httpMethod = httpMethod) 
            | Option.None -> Http.AsyncRequestString (req.url, headers = headers, httpMethod = httpMethod) 

        asyncResult {
            return! httpAsync
            |> ofAsync 
            |> AsyncResult.mapError mapError 
        }

    let api = {request = request}

    type RunApi<'T, 'TError> = ReaderAsyncResult.ReaderAsyncResult<RequestApi, 'T, 'TError> -> AsyncResult<'T, 'TError>
    let runApi<'T, 'TError> : RunApi<'T, 'TError> = ReaderAsyncResult.run api

    let post2<'T> (url: Url) (headers: Headers) (payload: Payload) : HttpResult<'T>  =       
        post2<'T> url headers payload |> runApi

    let post<'T> url payload = post2<'T> url [] payload
    
    let delete<'T> (url: Url) (headers: Headers) (payload: Payload) : HttpResult<'T>  =       
        delete<'T> url headers payload |> runApi


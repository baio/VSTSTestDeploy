namespace Request

open AsyncResult
open ReaderAsyncResult

type Url = string
type Headers = seq<string * string>
type QueryString = seq<string * string>

type Payload = 
    FormPayload of list<string * string> 
    | JsonPayload of obj
    | None

type HttpMethod = POST | PUT | PATCH | DELETE | GET 

type Request = {
    url: Url
    payload: Payload
    headers: Headers
    httpMethod: HttpMethod
}

type HttpError = 
    | WebError of System.Net.WebException 
    | UnknowError of obj

type HttpResult<'T> = AsyncResult<'T, HttpError>

type RequestApiRequest = Request -> HttpResult<string>

type RequestApi = {
    request : RequestApiRequest
}

type Post2<'T> = Url -> Headers -> Payload -> HttpResult<'T>

type Post<'T> = Url -> Payload -> HttpResult<'T>

type RequestResult<'T> = ReaderAsyncResult<RequestApi, 'T, HttpError>

[<AutoOpen>]
module Utils =
    
    let toObj x = x :> obj
    
    let toJsonPayload (x: 'T) =  x |> toObj |> JsonPayload 

module Request = 

    open Newtonsoft.Json
    
    let request<'T> (request: Request) : RequestResult<'T> = 
        ReaderAsyncResult(fun ({ request = req }) -> req request) |> ReaderAsyncResult.map JsonConvert.DeserializeObject<'T>
        
    let post2<'T> (url: Url) (headers: Headers) (payload: Payload) : RequestResult<'T> = 
        request<'T>({url = url; headers = headers; httpMethod = POST; payload = payload})
   
    let post<'T> (url: Url) (payload: Payload) : RequestResult<'T> = 
        post2<'T> url [] payload

    let delete<'T> (url: Url) (headers: Headers) (payload: Payload) : RequestResult<'T> = 
        request<'T>({url = url; headers = headers; httpMethod = DELETE; payload = payload})

    let get<'T> (url: Url) (headers: Headers) : RequestResult<'T> = 
        request<'T>({url = url; headers = headers; httpMethod = GET; payload = None})

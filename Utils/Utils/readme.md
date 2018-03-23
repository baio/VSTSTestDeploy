# Set of utilities to work with Net.Core in FP

## AsyncResult

Async + Result monad

`asyncResult` Computation Expressions

## ReaderAsyncResult

Reader + Async + Result monad

`readerAsyncResult` Computation Expressions

## Request

`type RequestResult<'T> = ReaderAsyncResult<RequestApi, 'T, HttpError>`

Send data as request and desrealize result

## HtppRequest

Run `RequestApi` with real Http request from `FSharp.Data.Http`

## Config

Utility to read .Net core [config](https://msdn.microsoft.com/en-us/magazine/mt632279.aspx)

`settings.json` or any json file + env vars
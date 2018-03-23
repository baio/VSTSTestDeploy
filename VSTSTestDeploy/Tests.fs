module Tests

open System
open Xunit
open AsyncResult

open Microsoft.Azure.Services.AppAuthentication
open Microsoft.Azure.KeyVault

let private getAzureKeyVaultClient () = 
    let azureServiceTokenProvider = new AzureServiceTokenProvider()
    let clbk = azureServiceTokenProvider.KeyVaultTokenCallback
    let authCallback = new KeyVaultClient.AuthenticationCallback(fun x y z -> clbk.Invoke(x, y, z))
    new KeyVaultClient(authCallback)

let private getSecret (keyVault: KeyVaultClient) keyVaultUrl secretName =
    sprintf "https://%s.vault.azure.net/secrets/%s" keyVaultUrl secretName
    |> keyVault.GetSecretAsync
    |> AsyncResult.ofTask

let private replace x y (str: string) = str.Replace(oldValue = x, newValue = y)
    
/// You must be connected to azure account, and you azure account must have access to the key vault
let getAzureKeyVaultSecrets2 repl url = 
     let keyVault = getAzureKeyVaultClient()
     let getSecret = repl >> getSecret keyVault url >> map (fun x -> x.Value)
     traverseA getSecret 

let getAzureKeyVaultSecrets = getAzureKeyVaultSecrets2 id

let getAzureKeyVaultSecretsConfigAligned = getAzureKeyVaultSecrets2 (replace ":" "--")

(*
    Read secrets from azure keyVault
    azureKeyVaultConfigName - name of the config param which contains name of the azure key vault
   
    secretNames - secret names to read
*)
let getConfig2 azureKeyVaultConfigName secretNames = 

    let config = Config.getConfig()

    let akvName = config.Item azureKeyVaultConfigName

    match isNull akvName with
    | true ->
        rtn []
    | false ->
        getAzureKeyVaultSecretsConfigAligned akvName secretNames
        
(*
    Before getting secrets from key vault will try to retrieve same secrets from settings and env
*)
let getConfig azureKeyVaultConfigName secretNames = 

    let config = Config.getConfig()

    let secrets = secretNames |> List.map (fun x -> (x, config.Item x)) |> List.filter (snd >> isNull >> not)

    let foundSecretNames = secrets |> List.map fst
    let foundSecretVals = secrets |> List.map snd

    let otherSecrets = secretNames |> List.except foundSecretNames

    List.append foundSecretVals <!> getConfig2 azureKeyVaultConfigName otherSecrets 

 
    
[<Fact>]
let ``My test`` () =
    getConfig "azureKeyVault:name" ["auth0--audience"; "azureKeyVault:name"]
    |> map (fun x ->
        System.Diagnostics.Debug.WriteLine(sprintf "%s" x.[0])
        System.Diagnostics.Debug.WriteLine(sprintf "%s" x.[1])
        Assert.True(x.Length = 2)
    ) 
    |> mapError (fun x -> Assert.Null(x))

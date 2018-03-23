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
    
    exn (secretNames |> String.concat ",") |> Error |> ofResult

    (*
    if List.length secretNames = 0 then  
        rtn []
    else
        let config = Config.getConfig()

        let akvName = config.Item azureKeyVaultConfigName

        match isNull akvName with
        | true ->
            rtn []
        | false ->
            getAzureKeyVaultSecretsConfigAligned akvName secretNames
    *)

let getSecretsFromConfig (config: Microsoft.Extensions.Configuration.IConfigurationRoot) f secretNames =
    let res = secretNames |> List.map (fun x -> (x, config.Item (f x))) |> List.filter (snd >> isNull >> not)
    res |> List.map fst, res |> List.map snd

(*
    Before getting secrets from key vault will try to retrieve same secrets from settings and env.
    For config names with ':' will be processed two times, first as is and next with ':' replaced to '--'
    For key vault in names always replace ':' with '--' 
*)
let getConfig azureKeyVaultConfigName secretNames = 

    let config = Config.getConfig()
    
    // get from config by the names as is
    let n1, v1 = secretNames |> getSecretsFromConfig config id

    // collect not found names
    let n2 = secretNames |> List.except n1

    // get from config by the names with replaces ":" to "--"
    let n3, v3 = n2 |> getSecretsFromConfig config (replace ":" "--")

    // collect all found names
    let n4 = List.append n1 n3

    // collect all found values
    let v4 = List.append v1 v3

    // collect stil not found names
    let n5 = secretNames |> List.except n4
    
    // collect all found
    List.append v4 <!> getConfig2 azureKeyVaultConfigName n5    

 
[<Fact>]
let ``My test`` () =
    getConfig "azureKeyVault:name" ["auth0:audience"; "azureKeyVault:name"]
    |> map (fun x ->
        System.Diagnostics.Debug.WriteLine(sprintf "%s" x.[0])
        System.Diagnostics.Debug.WriteLine(sprintf "%s" x.[1])
        Assert.True(x.Length = 2)
    ) 
    |> mapError (fun x -> Assert.Null(x))

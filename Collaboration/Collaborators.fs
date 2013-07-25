module Collaborators

type Collaborator = {
    emailAddress : string;
    identity : string;
    name : string;
    collaboratorType : CollaboratorType; }

    with static member Zero = { 
            emailAddress     = null; 
            identity         = null; 
            name             = null; 
            collaboratorType = Author }

and CollaboratorType = Author | Creator | Moderator | Owner | Participant


type ICollaboratorService = abstract member GetCollaborator : Tenant * string * CollaboratorType -> Collaborator option


module Http =
    
    open System
    open System.Text
    open System.Net
    open System.Net.Http
    open System.Xml.Linq
    open Newtonsoft.Json

    let service = {
        new ICollaboratorService with 
            member __.GetCollaborator (tenantId,identity,role) =                                                                     
                let requestUrl = sprintf "http://idovation/tenants/%s/users/%s/inRole/%s" (tenantId.ToString()) identity (role.ToString())
                let request = new HttpRequestMessage(HttpMethod.Get, requestUrl)                
                use client = new HttpClient()
                let response = client.SendAsync(request) |> Async.AwaitTask |> Async.RunSynchronously
                match response.StatusCode with
                | HttpStatusCode.OK ->                         
                    let json = response.Content.ReadAsStringAsync() |> Async.AwaitTask |> Async.RunSynchronously |> JsonConvert.DeserializeXNode
                    let get name = json.Element(XName.Get(name)).Value
                    { emailAddress     = get "emailAddress";
                      identity         = get "username";
                      name             = get ("firstName") + " " + get ("lastName");
                      collaboratorType = role; } |> Some
                | _ -> None                                
    }
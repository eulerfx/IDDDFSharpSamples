module Application    
                
open Forums         
open Forums.Forum
open Collaborators       
open Aggregate

let aggregate = {
    zero  = State.Zero;
    apply = apply;
    exec  = exec; }
       
let handler = Aggregate.makeHandler aggregate        

type ICommandMaterializer<'TMessage, 'TCommand> = abstract member materialzie : 'TMessage -> 'TCommand

type ICommand = interface end

type IExternalCommand<'TArg> = interface end //abstract member assemble : 'TArg -> Command option

type AssignModeratorToForumCommand = {
    tenantId    : string;
    forumId     : string;
    moderatorId : string; } with interface ICommand
            
type ChangeForumDescriptionCommand = {
    tenantId    : string;
    forumId     : string;
    description : string; } with interface ICommand

type ChangeForumSubjectCommand = {
    tenantId : string;
    forumId  : string;
    subject  : string; } with interface ICommand

type CloseForumCommand = {
    tenantId : string;
    forumId  : string;
    subject  : string; } with interface ICommand

type StartForumCommand = {
    tenantId    : string;
    creatorId   : string;
    moderatorId : string;
    subject     : string;
    description : string; } 

        with member command.assemble (collaboratorService:ICollaboratorService, forumId: unit -> ForumId) =
                let tenantId = Tenant(command.tenantId)
                let creator = collaboratorService.GetCollaborator(tenantId, command.creatorId, Creator)
                let moderator = collaboratorService.GetCollaborator(tenantId, command.moderatorId, Moderator)
                let subject = command.subject |> nonEmptyString
                let description = command.description |> nonEmptyString
                let exclusiveOwner = None
                let forumId = forumId ()
                match creator,moderator,subject,description with
                | Some creator,Some moderator,Some subject,Some description 
                    -> Start ((tenantId,forumId), creator, moderator, subject, description, exclusiveOwner) |> Some
                | _ -> None

                                                                                       


type AssignModeratorToForumCommand with
    member command.assemble (collaboratorService:ICollaboratorService) =
        let tenant = Tenant(command.tenantId)
        let moderator = collaboratorService.GetCollaborator(tenant, command.moderatorId, Moderator)
        match moderator with
        | Some moderator -> moderator |> AssignModerator |> Some
        | _ -> None

type ChangeForumDescriptionCommand with
    member command.assemble () =
        match command.description |> nonEmptyString with
        | Some desc -> desc |> ChangeDescription |> Some
        | _ -> None
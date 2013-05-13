module Tenant                                  

open User

type Tenant = {
    tenantId : TenantId;
    active : bool;
    description : string;
    name : string;
    invitations : RegistrationInvitation Set;
}

and RegistrationInvitation = {
    tenantId : TenantId;
    invitationId : string;
    description : string;
    duration : Duration;
}

let Zero = { tenantId = TenantId(""); active = false; name = null; description = null; invitations = Set.empty }

type Command = 
    | Create of TenantId * string * string * bool
    | Activate
    | Deactivate
    | OfferRegistrationInvitation of string
    | ProvisionGroup of string * string
    | ProvisionRole of string * string * bool
    | WithdrawInvitation of string

    
type Event =
    | Created of TenantId * string * string * bool
    | AdministratorRegistered of FullName * string * string
    | GroupProvisioned of string
    | Activated
    | Deactivated
    | RoleProvisioned of string
    | RegistrationInvitationReceived of RegistrationInvitation
    | InvitationWithdrawn of RegistrationInvitation


let apply tenant = 
    function
    | Created (tenantId,name,description,active) -> { tenant with tenantId = tenantId; name = name; description = description; active = active; }
    | GroupProvisioned groupName                 -> tenant
    | RoleProvisioned roleName                   -> tenant
    | Activated                                  -> { tenant with active = true }
    | Deactivated                                -> { tenant with active = false }
    | RegistrationInvitationReceived invite      -> { tenant with invitations = tenant.invitations |> Set.add invite }
    | InvitationWithdrawn invite                 -> { tenant with invitations = tenant.invitations |> Set.remove invite }
    | AdministratorRegistered _                  -> tenant




let findInvite tenant inviteId = tenant.invitations |> Seq.tryFind (fun i -> i.invitationId = inviteId)

let registerUser (tenant,inviteId,userName,password,enablement,person) =
    match findInvite tenant inviteId with
    | Some invite when invite.duration.IsAvailable -> 
        Some { tenantId = tenant.tenantId; userName = userName; password = password; enablement = enablement; person = person; }
    | _ -> None


let provisionRole (tenant:Tenant,name,description,supportsNesting) = Role.make (tenant.tenantId,name,description,supportsNesting)


let exec tenant = 
    function

    | Create (tenantId,name,description,active) -> Created(tenantId,name,description,active) |> Choice1Of2

    | Activate ->
        match tenant.active with
        | true -> ["Already active"] |> Choice2Of2
        | _ -> Activated |> Choice1Of2

    | Deactivate ->
        match tenant.active with
        | true -> Deactivated |> Choice1Of2
        | _ -> ["Already inactive."] |> Choice2Of2

    | OfferRegistrationInvitation description ->
        let invite = { tenantId = tenant.tenantId; invitationId = Guid.NewGuid().ToString(); description = description; duration = Duration.OpenEnded }
        RegistrationInvitationReceived invite |> Choice1Of2

    | WithdrawInvitation inviteId -> 
        let invite = findInvite tenant inviteId
        match invite with
        | Some invite -> InvitationWithdrawn(invite) |> Choice1Of2
        | None -> ["Invite not found."] |> Choice2Of2

    | ProvisionGroup (name,description) ->
        match tenant.active with
        | true ->
            let group = Group.make (tenant.tenantId,name,description)
            GroupProvisioned(name) |> Choice1Of2
        | _ -> ["Tenant is not active"] |> Choice2Of2

    | ProvisionRole (name,description,supportsNesting) ->
        match tenant.active with
        | true ->
            let role = Role.make (tenant.tenantId,name,description,supportsNesting)
            RoleProvisioned(name) |> Choice1Of2
        | _ -> ["Tenant is not active"] |> Choice2Of2
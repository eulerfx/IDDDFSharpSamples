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
    | Provision of TenantId * string * string * bool
    | Activate
    | Deactivate
    | OfferRegistrationInvitation of string
    | ProvisionGroup of string * string
    | ProvisionRole of string * string * bool
    | WithdrawInvitation of string

    
type Event =
    | Provisioned of TenantId * string * string * bool    
    | GroupProvisioned of string
    | Activated
    | Deactivated
    | RoleProvisioned of string
    | RegistrationInvitationReceived of RegistrationInvitation
    | InvitationWithdrawn of RegistrationInvitation
    | AdministratorRegistered of string * FullName * EmailAddress * string * EncryptedPassword


let apply tenant = 
    function
    | Provisioned (tenantId,name,description,active) -> { tenant with tenantId = tenantId; name = name; description = description; active = active; }
    | GroupProvisioned groupName                 -> tenant
    | RoleProvisioned roleName                   -> tenant
    | Activated                                  -> { tenant with active = true }
    | Deactivated                                -> { tenant with active = false }
    | RegistrationInvitationReceived invite      -> { tenant with invitations = tenant.invitations |> Set.add invite }
    | InvitationWithdrawn invite                 -> { tenant with invitations = tenant.invitations |> Set.remove invite }
    | AdministratorRegistered _                  -> tenant


let findInvite tenant inviteId = tenant.invitations |> Seq.tryFind (fun i -> i.invitationId = inviteId || i.description = inviteId)


module private Assert =
    let active tenant = validator (fun t -> t.active = false) ["The tenant is inactive."] tenant
    let inactive tenant = validator (fun t -> t.active = true) ["The tenant is already active."] tenant
    let notInvited (tenant,inviteId) = validator (fun t -> findInvite tenant inviteId |> Option.isNone) ["The tenant was already invited."] tenant
    let invited (tenant,inviteId) = validator (fun t -> findInvite tenant inviteId |> Option.isSome) ["The tenant doesn't have the specified invite."] tenant



let registerUser (tenant,inviteId,userName,password,enablement,person) =
    match findInvite tenant inviteId with
    | Some invite when invite.duration.IsAvailable -> 
        { tenantId = tenant.tenantId; userName = userName; password = password; enablement = enablement; person = person; } |> Choice1Of2
    | _ -> ["Could not find effective invite."] |> Choice2Of2


let provisionRole (tenant:Tenant,name,description,supportsNesting) = 
    Assert.active tenant <?> Role.make (tenant.tenantId,name,description,supportsNesting)


let exec tenant = 
    function

    | Provision (tenantId,name,description,active) -> Provisioned(tenantId,name,description,active) |> Choice1Of2

    | Activate -> Assert.inactive tenant <?> Activated

    | Deactivate -> Assert.active tenant <?> Deactivated

    | OfferRegistrationInvitation description -> 
        let invite = { tenantId = tenant.tenantId; invitationId = Guid.NewGuid().ToString(); description = description; duration = Duration.OpenEnded }
        Assert.active tenant <* Assert.notInvited (tenant,description) <?> RegistrationInvitationReceived invite

    | WithdrawInvitation inviteId -> 
        let invite = findInvite tenant inviteId
        match invite with
        | Some invite -> InvitationWithdrawn(invite) |> Choice1Of2
        | None -> ["Invite not found."] |> Choice2Of2

    | ProvisionGroup (name,description) ->
        Assert.active tenant <?> GroupProvisioned ((Group.make (tenant.tenantId,name,description)).name)
        
    | ProvisionRole (name,description,supportsNesting) ->
        Assert.active tenant <?> RoleProvisioned ((Role.make (tenant.tenantId,name,description,supportsNesting)).name)
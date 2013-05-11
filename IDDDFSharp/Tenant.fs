module Tenant
    
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
    duration : DateTime * DateTime;
}


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
        let invite = { tenantId = tenant.tenantId; invitationId = Guid.NewGuid().ToString(); description = description; duration = (DateTime.MinValue,DateTime.MinValue) }
        RegistrationInvitationReceived invite |> Choice1Of2

    | WithdrawInvitation inviteId -> 
        let invite = tenant.invitations |> Seq.tryFind (fun i -> i.invitationId = inviteId)
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




module Authorization =        

    let isUserInRole (roleNamed,userNamed,groupNamed) (user:User.User,roleName:string) =
        let role : Role.Role option = roleNamed (user.tenantId,roleName)        
        match role with
        | Some role ->

            let confirmUser (group:Group.Group,user:User.User) =
                let confirmedUser : User.User option = userNamed (group.tenantId,user.userName)     
                match confirmedUser with
                | Some user when user.enablement.enabled -> true
                | _ -> false

            let rec isUserInNestedGroup (group:Group.Group,user:User.User) =
            
                let isInNestedGroup (groupMember:Group.GroupMember) =
                    let nestedGroup : Group.Group option = groupNamed (groupMember.tenantId,groupMember.name)
                    match nestedGroup with
                    | Some nestedGroup -> Group.isMember (nestedGroup,user,confirmUser,isUserInNestedGroup)
                    | None -> false
                
                group.members 
                    |> Seq.filter (fun m -> m.memberType = Group.GroupMemberType.Group)
                    |> Seq.tryFind isInNestedGroup 
                    |> Option.isSome
                    
            Group.isMember (role.group,user,confirmUser,isUserInNestedGroup)

        | None -> false
        

    let isUserInRoleByUserName (roleNamed,userNamed,groupNamed) (tenantId:TenantId,userName:string,roleName:string) =
        match (tenantId,userName) |> userNamed with
        | Some user -> isUserInRole (roleNamed,userNamed,groupNamed) (user,roleName)
        | None -> false
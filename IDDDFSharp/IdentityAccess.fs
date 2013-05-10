module IdentityAccess

open System


type TenantId = TenantId of string

type EmailAddress = EmailAddress of string

type Telephone = Telephone of string

type PostalAddress = {
    street : string;
    city : string;
    state : string;
    zip : string;
    countryCode : string;
};

type FullName = {
    firstName : string;
    lastName : string;
};

type EventMeta = {
    version : int;
    occurredOn : DateTime;
    tenantId : string;
    userName : string;
}
    with static member Null = { version = 0; occurredOn = DateTime.MinValue; tenantId = null; userName = null }


module Tenant =
    
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
        | Activate
        | Deactivate
        | OfferRegistrationInvitation of string
        | ProvisionGroup of string * string
        | ProvisionRole of string * string * bool
        | WithDrawInvitation of string

    
    type Event =
        | AdministratorRegistered of EventMeta * FullName * string * string
        | GroupProvisioned of EventMeta
        | Activated of EventMeta
        | Deactivated of EventMeta
        | Provision of EventMeta



module User =
    
    type User = {
        tenantId : TenantId;
        enablement : Enablement;
        userName : string;
        password : string;
        person : Person;
    }
    
    and UserDescriptor = {
        tenantId : TenantId;
        emailAddress : string;
        userName : string;
    }
        with static member Null = { tenantId = TenantId(null); emailAddress = null; userName = null }
    
    and Enablement = {
        enabled : bool;
        validDuring : DateTime * DateTime;
    }
        with static member Indefinite = { enabled = true; validDuring = (DateTime.MinValue,DateTime.MinValue) }

    and Person = {
        tenantId : string;
        fullName : FullName;
        user : User;
    }

    and ContactInformation = {
        emailAddress : EmailAddress;
        postalAddress : PostalAddress;
        primaryPhone : Telephone;
        secondaryPhone : Telephone;
    }


    type Command =
        | ChangePassword of string * string
        | ChangeContactInformation of string
        | ChangeName of FullName
        | DefineEnablement of Enablement


    type Event =
        | ContactInformationChanged of EventMeta * ContactInformation
        | PersonNameChanged of EventMeta * FullName
        | UserEnablementChanged of EventMeta * Enablement
        | UserPasswordChanged of EventMeta
        | UserRegistered of EventMeta * EmailAddress




module Group =    

    let [<Literal>] internal RoleGroupPrefix = "ROLE-INTERNAL-GROUP: "    

    type Group = {        
        name : string;
        description : string;
        tenantId : TenantId;
        groupMembers : GroupMember Set;
    }
    and GroupMember = {
        tenantId : TenantId;
        name : string;
        memberType : GroupMemberType;
    }
    and GroupMemberType = Group | User
    
    type IsMemberGroup = Group -> GroupMember -> bool

    type Command =
        | Create of TenantId * string * string
        | AddGroupMember of Group * IsMemberGroup
        | AddGroupUser of User.User
        | RemoveGroupMember of Group
        | RemoveUserMember of User.User

    type Event =
        | Created of string
        | GroupMemberAdded of string * string
        | GroupMemberRemoved of string * string
        | GroupUserAdded of string * string
        | GroupUserRemoved of string * string
    

    let apply (group:Group) =
        let changeMembership change (memberName,memberType) = { group with groupMembers = group.groupMembers |> change { tenantId = group.tenantId; name = memberName; memberType = memberType } }
        let addMember = changeMembership Set.add
        let removeMember = changeMembership Set.remove
        function
        | Created (name)                    -> { group with name = name }
        | GroupMemberAdded (_,memberName)   -> addMember (memberName,Group)
        | GroupMemberRemoved (_,memberName) -> removeMember (memberName,Group)
        | GroupUserAdded (_,userName)       -> addMember (userName,User)
        | GroupUserRemoved (_,userName)     -> removeMember (userName,User)


    let exec (group:Group) =     
        let groupToGroupMember (group:Group) = { tenantId = group.tenantId; name = group.name; memberType = Group }
        let userToGroupMember (user:User.User) = { tenantId = user.tenantId; name = user.userName; memberType = User }
        let isInternalGroup = group.name.StartsWith(RoleGroupPrefix)        

        function                       
         
        | Create (tenantId,name,description) ->
            Created(name) |> Choice1Of2

        | AddGroupMember (groupToAdd,isMemberGroup) ->            
            match isInternalGroup with
            | true -> ["Internal group."] |> Choice2Of2 
            | _ -> 
                match group.groupMembers |> Set.contains (groupToAdd |> groupToGroupMember) with
                | false -> GroupMemberAdded(group.name, groupToAdd.name) |> Choice1Of2
                | _     -> ["Already member."] |> Choice2Of2                                 

        | AddGroupUser user ->             
            match isInternalGroup with
            | true -> ["Internal group."] |> Choice2Of2 
            | _ ->
                match group.groupMembers |> Set.contains (user |> userToGroupMember) with
                | false -> GroupUserAdded(group.name, user.userName) |> Choice1Of2
                | _ -> ["Already member."] |> Choice2Of2
            

        | RemoveGroupMember groupToRemove ->
            match isInternalGroup with
            | true -> ["Internal group."] |> Choice2Of2 
            | _ -> 
                match group.groupMembers |> Set.contains (groupToRemove |> groupToGroupMember) with
                | true -> GroupMemberRemoved(group.name, groupToRemove.name) |> Choice1Of2
                | _ -> ["Not member."] |> Choice2Of2

        | RemoveUserMember user ->
            match isInternalGroup with
            | true -> ["Internal group."] |> Choice2Of2
            | _ ->
                match group.groupMembers |> Set.contains (user |> userToGroupMember) with
                | true -> GroupUserRemoved(group.name, user.userName) |> Choice1Of2
                | _ -> ["Not member"] |> Choice2Of2
                           


            
        



module Role =       

    type Role = {
        tenantId : TenantId;
        name : string;
        description : string;
        supportsNesting : bool;
        group : Group.Group;
    }

    type Command =
        | Create of TenantId * string * string * bool
        | AssignGroup of Group.Group * Group.IsMemberGroup
        | AssignUser of User.User
        | UnassignGroup of Group.Group
        | UnassignUser of User.User
        
    type Event =
        | Created of string
        | GroupAssignedToRole of string * string
        | GroupUnassignedFromRole of string * string
        | RoleProvisioned of string
        | UserAssignedToRole of string * string
        | UserUnassignedFromRole of string * string

   
    let apply role =
        let applyToGroup event = { role with group = Group.apply role.group event }
        function
        | Created (name) -> { role with name = name }        
        | GroupAssignedToRole (roleName,groupName)     -> Group.GroupMemberAdded(role.group.name, groupName) |> applyToGroup
        | GroupUnassignedFromRole (roleName,groupName) -> Group.GroupMemberRemoved(role.group.name, groupName) |> applyToGroup
        | UserAssignedToRole (roleName,userName)       -> Group.GroupUserAdded(role.group.name, userName) |> applyToGroup
        | UserUnassignedFromRole (roleName,userName)   -> Group.GroupUserRemoved(role.group.name, userName) |> applyToGroup
        | RoleProvisioned (roleName)                   -> role                


    let exec role =     
        let execGroup = Group.exec role.group

        function
   
        | Create (tenantId,name,description,supportsNesting) ->
            let group = { 
                Group.Group.tenantId = tenantId; 
                Group.Group.name = Group.RoleGroupPrefix + Guid.NewGuid().ToString(); 
                Group.Group.description = sprintf "Role backing group for: %s" name; 
                Group.Group.groupMembers = Set.empty }
            Created(name) |> Choice1Of2

        | AssignGroup (group,isMemberGroup) -> 
            match (group,isMemberGroup) |> Group.AddGroupMember |> execGroup with
            | Choice1Of2 _ -> GroupAssignedToRole(role.name, group.name) |> Choice1Of2
            | Choice2Of2 e -> e |> Choice2Of2

        | AssignUser user ->
            match user |> Group.AddGroupUser |> execGroup with
            | Choice1Of2 _ -> UserAssignedToRole(role.name, user.userName) |> Choice1Of2
            | Choice2Of2 e -> e |> Choice2Of2

        | UnassignGroup group ->
            match group |> Group.RemoveGroupMember |> execGroup with
            | Choice1Of2 _ -> GroupUnassignedFromRole(role.name, group.name) |> Choice1Of2
            | Choice2Of2 e -> Choice2Of2 e

        | UnassignUser user ->
            match user |> Group.RemoveUserMember |> execGroup with
            | Choice1Of2 _ -> UserUnassignedFromRole(role.name, user.userName) |> Choice1Of2
            | Choice2Of2 e -> Choice2Of2 e
        
        
    
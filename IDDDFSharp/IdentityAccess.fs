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
        | AddGroup of Group * IsMemberGroup
        | AddUser of User.User
        | RemoveGroup of Group
        | RemoveUser of User.User

    type Event =
        | GroupMemberAdded of string * string
        | GroupMemberRemoved of string * string
        | GroupUserAdded of string * string
        | GroupUserRemoved of string * string
    

    let apply (group:Group) =
        let changeMembership change (memberName,memberType) = { group with groupMembers = group.groupMembers |> change { tenantId = group.tenantId; name = memberName; memberType = memberType } }
        let addMember = changeMembership Set.add
        let removeMember = changeMembership Set.remove
        function
        | GroupMemberAdded (_,memberName)   -> addMember (memberName,Group)
        | GroupMemberRemoved (_,memberName) -> removeMember (memberName,Group)
        | GroupUserAdded (_,userName)       -> addMember (userName,User)
        | GroupUserRemoved (_,userName)     -> removeMember (userName,User)


    let exec (group:Group) =     
        let groupToGroupMember (group:Group) = { tenantId = group.tenantId; name = group.name; memberType = Group }
        let userToGroupMember (user:User.User) = { tenantId = user.tenantId; name = user.userName; memberType = User }
        let isInternalGroup = group.name.StartsWith(RoleGroupPrefix)        

        function                       
         
        | AddGroup (groupToAdd,isMemberGroup) ->            
            match isInternalGroup with
            | true -> ["Internal group."] |> Choice2Of2 
            | _ -> 
                match group.groupMembers |> Set.contains (groupToAdd |> groupToGroupMember) with
                | false -> GroupMemberAdded(group.name, groupToAdd.name) |> Choice1Of2
                | _     -> ["Already member."] |> Choice2Of2                                 

        | AddUser user ->             
            match isInternalGroup with
            | true -> ["Internal group."] |> Choice2Of2 
            | _ ->
                match group.groupMembers |> Set.contains (user |> userToGroupMember) with
                | false -> GroupUserAdded(group.name, user.userName) |> Choice1Of2
                | _ -> ["Already member."] |> Choice2Of2
            

        | RemoveGroup groupToRemove ->
            match isInternalGroup with
            | true -> ["Internal group."] |> Choice2Of2 
            | _ -> 
                match group.groupMembers |> Set.contains (groupToRemove |> groupToGroupMember) with
                | true -> GroupMemberRemoved(group.name, groupToRemove.name) |> Choice1Of2
                | _ -> ["Not member."] |> Choice2Of2

        | RemoveUser user ->
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
        | AssignGroup of Group.Group * Group.IsMemberGroup
        | AssignUser of User.User
        | UnassignGroup of Group.Group
        | UnassignUser of User.User
        
    type Event =
        | GroupAssignedToRole of EventMeta * string * string
        | GroupUnassignedFromRole of EventMeta * string
        | RoleProvisioned of EventMeta * string
        | UserAssignedToRole of EventMeta * string * string * string
        | UserUnassignedFromRole of EventMeta * string

    
//    let apply (role:Role) = function
//    | GroupAssignedToRole(_,roleName,groupName) -> { role with group = role.group.ad
//
//    let exec (role:Role) = function
//    | AssignGroup (group,isMemberGroup) -> GroupAssignedToRole(EventMeta.Null, group.name)
//    | _ ->  UserUnassignedFromRole(EventMeta.Null, null)       
        
        
    
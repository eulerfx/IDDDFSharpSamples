module Access

open System

type set<'T> = System.Collections.Generic.HashSet<'T>


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
    version : string;
    occurredOn : DateTime;
    tenantId : string;
    userName : string;
}


module Tenant =
    
    type Tenant = {
        tenantId : TenantId;
        active : bool;
        description : string;
        name : string;
        invitations : RegistrationInvitation set;
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

    let [<Literal>] RoleGroupPrefix = "ROLE-INTERNAL-GROUP: "    

    type Group = {        
        name : string;
        description : string;
        tenantId : TenantId;
        groupMembers : GroupMember set;
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
        | GroupAdded of EventMeta * string * string
        | GroupRemoved of EventMeta * string * string
        | UserAdded of EventMeta * string
        | UserRemoved of EventMeta * string
    


    let exec (group:Group) = 
    
        let toGroupMember (group:Group) = { tenantId = group.tenantId; name = group.name; memberType = Group }

        let userGroupMember (user:User.User) = { tenantId = group.tenantId; name = group.name; memberType = User }

        let isInternalGroup (group:Group) = group.name.StartsWith(RoleGroupPrefix)

        function                        
        | AddGroup (groupToAdd,isMemberGroup) ->
            match group.groupMembers.Add(groupToAdd |> toGroupMember),(isInternalGroup group) with
            | true,true ->                 
                ()
            | _ -> ()

        | AddUser user -> 
            
            ()
            

        | _ -> ()


            
        



module Role =       

    type Role = {
        name : string;
        description : string;
        supportsNesting : bool;
        tenantId : TenantId;
    }

    type Command =
        | AssignGroup of Group.Group * Group.IsMemberGroup
        | AssignUser of User.User
        | UnassignGroup of Group.Group
        | UnassignUser of User.User
        
    type Event =
        | GroupAssignedToRole of EventMeta * string
        | GroupUnassignedFromRole of EventMeta * string
        | RoleProvisioned of EventMeta * string
        | UserAssignedToRole of EventMeta * string * string * string
        | UserUnassignedFromRole of EventMeta * string            
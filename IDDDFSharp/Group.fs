module Group

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
    
let make (tenantId,name,description) = { tenantId = tenantId; name = name; description = description; groupMembers = Set.empty }

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
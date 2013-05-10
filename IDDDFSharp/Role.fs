module Role      

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

let make (tenantId,name,description,supportsNesting) = 
    let groupName = Group.RoleGroupPrefix + Guid.NewGuid().ToString()
    let groupDesc = sprintf "Role backing group for: %s" name
    let group = Group.make (tenantId,groupName,groupDesc)
    { tenantId = tenantId; name = name; description = description; supportsNesting = supportsNesting; group = group; }

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
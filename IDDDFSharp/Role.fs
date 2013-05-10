module Role      

type Role = {
    tenantId : TenantId;
    name : string;
    description : string;
    supportsNesting : bool;
    group : Group.Group;
}

let Zero = { tenantId = TenantId(""); name = ""; description = ""; supportsNesting = false; group = Group.Zero; }

type Command =
    | Create of TenantId * string * string * bool
    | AssignGroup of Group.Group * Group.IsMemberGroup
    | AssignUser of User.User
    | UnassignGroup of Group.Group
    | UnassignUser of User.User
        
type Event =
    | Created of TenantId * string * string * bool * string
    | GroupAssignedToRole of Group.GroupMember
    | GroupUnassignedFromRole of Group.GroupMember    
    | UserAssignedToRole of Group.GroupMember
    | UserUnassignedFromRole of Group.GroupMember
    | RoleProvisioned of string

let private makeBackingGroup (tenantId,roleName,groupNameId) =
    let groupName = Group.RoleGroupPrefix + groupNameId
    let groupDesc = sprintf "Role backing group for: %s" roleName
    Group.make (tenantId,groupName,groupDesc)

let apply role =
    let applyToGroup event = { role with group = Group.apply role.group event }
    function
    | Created (tenantId,name,description,supportsNesting,groupNameId) ->
        let group = makeBackingGroup (tenantId,name,groupNameId)
        { tenantId = tenantId; name = name; description = description; supportsNesting = supportsNesting; group = group }      
    | GroupAssignedToRole groupMember                               -> groupMember |> Group.GroupMemberAdded |> applyToGroup
    | GroupUnassignedFromRole groupMember                           -> groupMember |> Group.GroupMemberRemoved |> applyToGroup
    | UserAssignedToRole groupMember                                -> groupMember |> Group.GroupUserAdded |> applyToGroup
    | UserUnassignedFromRole groupMember                            -> groupMember |> Group.GroupUserRemoved |> applyToGroup
    | RoleProvisioned roleName                                      -> role                


let make (tenantId,name,description,supportsNesting) = 
    Created (tenantId,name,description,supportsNesting,Guid.NewGuid().ToString()) |> apply Zero

let exec role =     

    let execGroup = Group.exec role.group

    function
   
    | Create (tenantId,name,description,supportsNesting) -> (tenantId,name,description,supportsNesting,Guid.NewGuid().ToString()) |> Created |> Choice1Of2

    | AssignGroup (group,isMemberGroup) -> 
        match (group,isMemberGroup) |> Group.AddGroupMember |> execGroup with
        | Choice1Of2 e -> group |> Group.groupToGroupMember |> GroupAssignedToRole |> Choice1Of2
        | Choice2Of2 e -> e |> Choice2Of2

    | AssignUser user ->
        match user |> Group.AddGroupUser |> execGroup with
        | Choice1Of2 _ -> user |> Group.userToGroupMember |> UserAssignedToRole |> Choice1Of2
        | Choice2Of2 e -> e |> Choice2Of2

    | UnassignGroup group ->
        match group |> Group.RemoveGroupMember |> execGroup with
        | Choice1Of2 _ -> group |> Group.groupToGroupMember |> GroupUnassignedFromRole |> Choice1Of2
        | Choice2Of2 e -> Choice2Of2 e

    | UnassignUser user ->
        match user |> Group.RemoveUserMember |> execGroup with
        | Choice1Of2 _ -> user |> Group.userToGroupMember |> UserUnassignedFromRole |> Choice1Of2
        | Choice2Of2 e -> Choice2Of2 e
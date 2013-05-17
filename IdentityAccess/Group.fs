module Group

let [<Literal>] internal RoleGroupPrefix = "ROLE-INTERNAL-GROUP: "    

type Group = {        
    tenantId : TenantId;
    name : string;
    description : string;    
    members : GroupMember Set;
}

and GroupMember = {
    tenantId : TenantId;
    name : string;
    memberType : GroupMemberType;
}

and GroupMemberType = Group | User
 
let Zero = { tenantId = TenantId(""); name = ""; description = ""; members = Set.empty; }
    
type IsMemberGroup = Group * GroupMember -> bool
type ConfirmUser = Group * User.User -> bool
type IsUserInNestedGroup = Group * User.User -> bool

type Command =
    | Create of TenantId * string * string
    | AddGroupMember of Group * IsMemberGroup
    | AddGroupUser of User.User
    | RemoveGroupMember of Group
    | RemoveUserMember of User.User

type Event =
    | Created of TenantId * string * string
    | GroupMemberAdded of GroupMember
    | GroupMemberRemoved of GroupMember
    | GroupUserAdded of GroupMember
    | GroupUserRemoved of GroupMember
   

let apply (group:Group) =
    let change change groupMember = { group with members = group.members |> change groupMember }
    let add = change Set.add
    let remove = change Set.remove
    function
    | Created (tenantId,name,description) -> { tenantId = tenantId; name = name; description = description; members = Set.empty }
    | GroupMemberAdded groupMember        -> groupMember |> add 
    | GroupMemberRemoved groupMember      -> groupMember |> remove 
    | GroupUserAdded groupMember          -> groupMember |> add 
    | GroupUserRemoved groupMember        -> groupMember |> remove 


let make (tenantId,name,description) = 
    { tenantId = tenantId; name = name; description = description; members = Set.empty } |> Success

let groupToGroupMember (group:Group) = 
    { tenantId = group.tenantId; name = group.name; memberType = Group }

let userToGroupMember (user:User.User) = 
    { tenantId = user.tenantId; name = user.userName; memberType = User }

let isMember (group,user,confirmUser:ConfirmUser,isUserInNestedGroup:IsUserInNestedGroup) =
    match group.members |> Set.contains (user |> userToGroupMember) with
    | true  -> (group,user) |> confirmUser
    | false -> (group,user) |> isUserInNestedGroup


let exec (group:Group) = 
    
    let assertNonInternal = validator (fun (g:Group) -> g.name.StartsWith(RoleGroupPrefix) |> not) ["The group is internal."] group
    let assertUniqueMember groupMember = validator (fun g -> g.members |> Set.contains groupMember |> not) ["The member is already part of the group."] group
    let assertMemberExists groupMember = validator (fun g -> g.members |> Set.contains groupMember) ["The member is not part of the group."] group
    
    function                       
         
    | Create (tenantId,name,description) -> Created(tenantId,name,description) |> Success

    | AddGroupMember (groupToAdd,isMemberGroup) ->
        let groupMember = groupToAdd |> groupToGroupMember
        assertNonInternal <* assertUniqueMember groupMember <?> GroupMemberAdded groupMember

    | AddGroupUser user ->             
        let groupMember = user |> userToGroupMember
        assertNonInternal <* assertUniqueMember groupMember <?> GroupMemberAdded groupMember            

    | RemoveGroupMember groupToRemove ->
        let groupMember = groupToRemove |> groupToGroupMember
        assertNonInternal <* assertMemberExists groupMember <?> GroupMemberRemoved groupMember

    | RemoveUserMember user ->
        let groupMember = user |> userToGroupMember
        assertNonInternal <* assertMemberExists groupMember <?> GroupMemberRemoved groupMember
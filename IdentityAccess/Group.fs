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


let make (tenantId,name,description) = { tenantId = tenantId; name = name; description = description; members = Set.empty }

let groupToGroupMember (group:Group) = { tenantId = group.tenantId; name = group.name; memberType = Group }

let userToGroupMember (user:User.User) = { tenantId = user.tenantId; name = user.userName; memberType = User }

let isMember (group,user,confirmUser,isUserInNestedGroup) : bool =    
    let isMember = group.members |> Set.contains (user |> userToGroupMember)
    match isMember with
    | true  -> (group,user) |> confirmUser
    | false -> (group,user) |> isUserInNestedGroup


let exec (group:Group) =     
        
    let isInternalGroup = group.name.StartsWith(RoleGroupPrefix)        

    function                       
         
    | Create (tenantId,name,description) -> Created(tenantId,name,description) |> Choice1Of2

    | AddGroupMember (groupToAdd,isMemberGroup) ->            
        match isInternalGroup with
        | true -> ["Internal group."] |> Choice2Of2 
        | _ -> 
            let groupMember = groupToAdd |> groupToGroupMember
            match group.members |> Set.contains groupMember with
            | false -> groupMember |> GroupMemberAdded |> Choice1Of2
            | _     -> ["Already member."] |> Choice2Of2                                 

    | AddGroupUser user ->             
        match isInternalGroup with
        | true -> ["Internal group."] |> Choice2Of2 
        | _ ->
            let groupMember = user |> userToGroupMember
            match group.members |> Set.contains groupMember with
            | false -> groupMember |> GroupUserAdded |> Choice1Of2
            | _ -> ["Already member."] |> Choice2Of2
            

    | RemoveGroupMember groupToRemove ->
        match isInternalGroup with
        | true -> ["Internal group."] |> Choice2Of2 
        | _ -> 
            let groupMember = groupToRemove |> groupToGroupMember
            match group.members |> Set.contains groupMember with
            | true -> groupMember |> GroupMemberRemoved |> Choice1Of2
            | _ -> ["Not member."] |> Choice2Of2

    | RemoveUserMember user ->
        match isInternalGroup with
        | true -> ["Internal group."] |> Choice2Of2
        | _ ->
            let groupMember = user |> userToGroupMember
            match group.members |> Set.contains groupMember with
            | true -> groupMember |> GroupUserRemoved |> Choice1Of2
            | _ -> ["Not member"] |> Choice2Of2
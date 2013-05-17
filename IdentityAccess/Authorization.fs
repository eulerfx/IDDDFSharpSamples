module Authorization

open Role
open User
open Group

let isUserInRole (roleNamed,userNamed,groupNamed) (user:User.User,roleName) =
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
        

let isUserInRoleByUserName (roleNamed,userNamed,groupNamed) (tenantId:TenantId,userName,roleName) =
    match (tenantId,userName) |> userNamed with
    | Some user -> isUserInRole (roleNamed,userNamed,groupNamed) (user,roleName)
    | None -> false
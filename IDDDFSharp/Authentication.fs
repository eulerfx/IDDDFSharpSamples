module Authentication

open Tenant
open User

let authenticate (tenantOfId,userFromCredentials,encrypt) (tenantId:TenantId,userName:string,password:string) =    
    let tenant : Tenant option = tenantOfId tenantId
    match tenant with
    | Some tenant when tenant.active ->
         let encryptedPassword : string = password |> encrypt
         let user : User option = (tenantId,userName,encryptedPassword) |> userFromCredentials
         match user with
         | Some user when user.enablement.enabled -> user.Descriptor
         | _ -> User.UserDescriptor.Null
    | _ -> User.UserDescriptor.Null        
        

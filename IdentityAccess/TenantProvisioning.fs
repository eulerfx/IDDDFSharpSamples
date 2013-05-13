module TenantProvisioning



type ActionBuilder() =
        
    member x.Bind(expr, func) =
        match expr with
        | Choice1Of2 r -> func r
        | Choice2Of2 e -> e |> Choice2Of2

    member x.Return(value) = value |> Choice1Of2

    member x.ReturnFrom(value) = value

    member x.Zero() = Choice1Of2 ()

    member x.Run(expr) = expr

    member x.Delay(func) = 
        match func() with
        | Choice1Of2 r -> r |> Choice1Of2
        | Choice2Of2 e -> e |> Choice2Of2



let action = new ActionBuilder()




open Tenant
open User


type Event = Tenant of Tenant.Event | User of User.Event | Role of Role.Event


let execApply tenant command = action {
    let! e = Tenant.exec tenant command
    return (Tenant.apply tenant e,e)
}

let provision (name,description,adminName,emailAddress,postalAddress,primaryPhone,secondaryPhone) = action {   
    
    let! (tenant,e0) = (TenantId(""),name,description,true) |> Provision |> execApply Tenant.Zero

    let! (tenant,e1) = "init" |> OfferRegistrationInvitation |> execApply tenant
    
    let person = {        
        fullName = adminName;
        contact = { emailAddress = EmailAddress(emailAddress);
                    postalAddress = postalAddress;
                    primaryPhone = primaryPhone;
                    secondaryPhone = secondaryPhone; } }
    
    let! admin = Tenant.registerUser (tenant,null,"admin",EncryptedPassword(""),Enablement.Indefinite,person)
           
    let! (tenant,e2) = "init" |> WithdrawInvitation |> execApply tenant

    let! adminRole = Tenant.provisionRole (tenant, "Administrator", "Default " + tenant.name + " administrator", false)           
    let e3 = AdministratorRegistered(tenant.name,adminName,person.contact.emailAddress,admin.userName,admin.password)

    let! e4 = admin |> Role.AssignUser |> Role.exec adminRole            
        
    return (e0,e1,e2,e3,e4)

    //return [ Tenant(e); Tenant(e2); Role(e3); Tenant(AdministratorRegistered(tenant.name, adminName, person.contact.emailAddress, admin.userName, admin.password)); Tenant(Provisioned(tenantId,name,description,false)); ]
}
module TenantProvisioning

open Tenant
open User

let provision (name,description,adminName,emailAddress,postalAddress,primaryPhone,secondaryPhone) = action {   
    
    let exec tenant command = action {
        let! e = Tenant.exec tenant command
        return (Tenant.apply tenant e,e)
    }
        
    let! (tenant,tenantProvisioned) = (TenantId(Guid.NewGuid().ToString()),name,description,true) |> Provision |> exec Tenant.Zero

    let! (tenant,registrationOfferReceived) = "init" |> OfferRegistrationInvitation |> exec tenant
    
    let person = {        
        fullName = adminName;
        contact = { emailAddress = EmailAddress(emailAddress);
                    postalAddress = postalAddress;
                    primaryPhone = primaryPhone;
                    secondaryPhone = secondaryPhone; } }
    
    let! admin = Tenant.registerUser (tenant,null,"admin",EncryptedPassword(""),Enablement.Indefinite,person)
           
    let! (tenant,invitationWithdrawn) = "init" |> WithdrawInvitation |> exec tenant

    let! (adminRole,adminRegistered) = 
        Tenant.provisionRole (tenant, "Administrator", "Default " + tenant.name + " administrator", false) 
        |> Validator.map 
            (fun role -> (role,
                          AdministratorRegistered (tenant.name,adminName,person.contact.emailAddress,admin.userName,admin.password)))                                   

    let! userAssignedToRole = admin |> Role.AssignUser |> Role.exec adminRole            
        
    return (tenantProvisioned,
            registrationOfferReceived,
            invitationWithdrawn,
            adminRegistered,
            userAssignedToRole)

}



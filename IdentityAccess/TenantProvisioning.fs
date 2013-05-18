module TenantProvisioning

open Tenant
open User
open Role

// TODO: create cross-aggregate composition operators as alternative to computation workflow
// TODO: implement event persistence and generalized execution context


//type Aggregate<'TState, 'TCommand, 'TEvent> = {
//    zero  : 'TState;
//    apply : 'TState -> 'TEvent -> 'TState;
//    exec  : 'TState -> 'TCommand -> Result<'TEvent, string list>
//}
//
//let execApply aggregate state command = action {
//    let! e = aggregate.exec state command
//    return aggregate.apply state e,e
//}
//
//let tenantAggregate = { zero = Tenant.Zero; apply = Tenant.apply; exec = Tenant.exec }
//
//let roleAggregate = { zero = Role.Zero; apply = Role.apply; exec = Role.exec }


(*
Provision
OfferRegistrationInvitation
registerUser
WithdrawInvitation
provisionRole
AssignUser
*)


let provision (name,description,adminName,emailAddress,postalAddress,primaryPhone,secondaryPhone) = action {   
    
    let exec tenant command = action {
        let! e = Tenant.exec tenant command
        return (Tenant.apply tenant e,e)
    }
        
    let! (tenant,tenantProvisioned) = 
         (TenantId(Guid.NewGuid().ToString()),name,description,true)
             |> Provision 
             |> exec Tenant.Zero

    let! (tenant,registrationOfferReceived) = "init" |> OfferRegistrationInvitation |> exec tenant
    
    let person = { fullName = adminName;
                   contact  = { emailAddress   = EmailAddress(emailAddress);
                                postalAddress  = postalAddress;
                                primaryPhone   = primaryPhone;
                                secondaryPhone = secondaryPhone; } }
    
    let! admin = Tenant.registerUser (tenant,null,"admin",EncryptedPassword("password"),Enablement.Indefinite,person)
           
    let! (tenant,invitationWithdrawn) = "init" |> WithdrawInvitation |> exec tenant

    let! (adminRole,adminRegistered) = 
        Tenant.provisionRole (tenant, "Administrator", "Default " + tenant.name + " administrator", false) 
        <**>
        AdministratorRegistered (tenant.name,adminName,person.contact.emailAddress,admin.userName,admin.password)

    let! userAssignedToRole = admin |> AssignUser |> Role.exec adminRole            
        
    return tenantProvisioned,
           registrationOfferReceived,
           invitationWithdrawn,
           adminRegistered,
           userAssignedToRole
}



module TenantProvisioning

open Tenant
open User

type Event = Tenant of Tenant.Event | User of User.Event | Role of Role.Event

let provision (name,description,adminName,emailAddress,postalAddress,primaryPhone,secondaryPhone) =        
    let tenantId = TenantId("")
    let tenant = { tenantId = tenantId; name = name; description = description; active = true; invitations = Set.empty }
    let e = "init" |> OfferRegistrationInvitation |> Tenant.exec tenant
    match e with
    | Choice1Of2 e ->
        let person = {        
            fullName = adminName;
            contact = { emailAddress = EmailAddress(emailAddress);
                        postalAddress = postalAddress;
                        primaryPhone = primaryPhone;
                        secondaryPhone = secondaryPhone; } }
        let admin = Tenant.registerUser (tenant, null, "admin", EncryptedPassword(""), Enablement.Indefinite, person)
        match admin with
        | Some admin ->                    
            let e2 = "init" |> WithdrawInvitation |> Tenant.exec tenant
            match e2 with
            | Choice1Of2 e2 ->
                let adminRole = Tenant.provisionRole (tenant, "Administrator", "Default " + tenant.name + " administrator", false)           
                let e3 = admin |> Role.AssignUser |> Role.exec adminRole            
                match e3 with
                | Choice1Of2 e3 -> 
                  [ Tenant(e); 
                    Tenant(e2); 
                    Role(e3); 
                    Tenant(AdministratorRegistered(adminName,"","")); 
                    Tenant(Created(tenantId,name,description,false)); ]  |> Choice1Of2
                | Choice2Of2 e -> e |> Choice2Of2
            | Choice2Of2 e -> e |> Choice2Of2        
        | None -> ["Could not register admin."] |> Choice2Of2
    | Choice2Of2 e -> e |> Choice2Of2

    // create tenant        
    // create admin user
    // withdraw invite
    // create admin role
    // assign admin to admin role
    // raise tenant provisioned event
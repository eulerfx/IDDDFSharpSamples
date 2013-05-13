module User
    
type User = {
    tenantId : TenantId;
    enablement : Enablement;
    userName : string;
    password : EncryptedPassword;
    person : Person;
}
    with member x.Descriptor = { tenantId = x.tenantId; userName = x.userName; emailAddress = x.person.contact.emailAddress }   
    
and UserDescriptor = {
    tenantId : TenantId;
    emailAddress : EmailAddress;
    userName : string;
}
    with static member Null = { tenantId = TenantId(null); emailAddress = EmailAddress(""); userName = null }
    
and Enablement = {
    enabled : bool;
    validDuring : Duration;
}
    with static member Indefinite = { enabled = true; validDuring = Duration.OpenEnded }

and Person = {
    fullName : FullName;
    contact : ContactInformation;
}

and ContactInformation = {
    emailAddress : EmailAddress;
    postalAddress : PostalAddress;
    primaryPhone : Telephone;
    secondaryPhone : Telephone;
}

and EncryptedPassword = EncryptedPassword of string


type Command =
    | Register of TenantId * string * string * Enablement * Person
    | ChangePassword of string * string
    | ChangeContactInformation of ContactInformation
    | ChangeName of FullName
    | DefineEnablement of Enablement


type Event =
    | UserRegistered of TenantId * string * EncryptedPassword * Enablement * Person
    | ContactInformationChanged of ContactInformation
    | PersonNameChanged of FullName
    | UserEnablementChanged of Enablement
    | UserPasswordChanged of EncryptedPassword    


let apply user = 
    function
    | UserRegistered (tenantId,userName,password,enablement,person) -> { tenantId = tenantId; userName = userName; password = password; enablement = enablement; person = person }
    | UserPasswordChanged password                                  -> { user with password = password }
    | UserEnablementChanged enablement                              -> { user with enablement = enablement }
    | ContactInformationChanged contact                             -> { user with person = { user.person with contact = contact } }
    | PersonNameChanged name                                        -> { user with person = { user.person with fullName = name } }

let exec (user:User) = 
    let encrypt (password:string) = EncryptedPassword(password)
    let isWeak (password:string) = false
    function
    | Register (tenantId,userName,password,enablement,person) -> 
        let password = password |> encrypt
        UserRegistered(tenantId,userName,password,enablement,person) |> Choice1Of2
    | ChangePassword (current,changeTo) ->         
        match user.password = (current |> encrypt) with
        | true ->
            match changeTo |> isWeak with
            | true -> ["Password is weak."] |> Choice2Of2
            | _ -> changeTo |> encrypt |> UserPasswordChanged |> Choice1Of2
        | _ -> ["Current password doesn't match."] |> Choice2Of2
    | ChangeContactInformation contact                        -> ContactInformationChanged(contact) |> Choice1Of2
    | ChangeName name                                         -> PersonNameChanged(name) |> Choice1Of2
    | DefineEnablement enablement                             -> UserEnablementChanged(enablement) |> Choice1Of2

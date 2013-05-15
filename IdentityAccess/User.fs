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
    let encrypt password = EncryptedPassword(password)
    let assertCurrentPasswordMatch (current,changeTo) = validator (fun p -> p = (changeTo |> encrypt)) ["Invalid current password specified."] current
    let assertStrongPassword password = validator (fun _ -> true) ["Password too weak."] password
    function
    | Register (tenantId,userName,password,enablement,person) -> 
        let password = password |> encrypt
        UserRegistered(tenantId,userName,password,enablement,person) |> Choice1Of2
    | ChangePassword (current,changeTo) ->
        assertCurrentPasswordMatch (user.password,current) 
        <* assertStrongPassword(changeTo) 
        <?> UserPasswordChanged(changeTo |> encrypt)
    | ChangeContactInformation contact -> ContactInformationChanged(contact) |> Choice1Of2
    | ChangeName name -> PersonNameChanged(name) |> Choice1Of2
    | DefineEnablement enablement -> UserEnablementChanged(enablement) |> Choice1Of2

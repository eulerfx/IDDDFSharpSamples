module User
    
type User = {
    tenantId : TenantId;
    enablement : Enablement;
    userName : string;
    password : string;
    person : Person;
}
    
and UserDescriptor = {
    tenantId : TenantId;
    emailAddress : string;
    userName : string;
}
    with static member Null = { tenantId = TenantId(null); emailAddress = null; userName = null }
    
and Enablement = {
    enabled : bool;
    validDuring : DateTime * DateTime;
}
    with static member Indefinite = { enabled = true; validDuring = (DateTime.MinValue,DateTime.MinValue) }

and Person = {
    tenantId : string;
    fullName : FullName;
    user : User;
    contact : ContactInformation;
}

and ContactInformation = {
    emailAddress : EmailAddress;
    postalAddress : PostalAddress;
    primaryPhone : Telephone;
    secondaryPhone : Telephone;
}


type Command =
    | Register of TenantId * string * string * Enablement * Person
    | ChangePassword of string * string
    | ChangeContactInformation of ContactInformation
    | ChangeName of FullName
    | DefineEnablement of Enablement


type Event =
    | ContactInformationChanged of ContactInformation
    | PersonNameChanged of FullName
    | UserEnablementChanged of Enablement
    | UserPasswordChanged of string
    | UserRegistered of EmailAddress


let apply user = 
    function
    | UserRegistered(emailAddress)       -> { user with person = { user.person with contact = { user.person.contact with emailAddress = emailAddress } } }
    | UserPasswordChanged(userName)      -> user
    | UserEnablementChanged(enablement)  -> { user with enablement = enablement }
    | ContactInformationChanged(contact) -> { user with person = { user.person with contact = contact } }
    | PersonNameChanged(name)            -> { user with person = { user.person with fullName = name } }

let exec (user:User) = 
    function
    | Register (tenantId,userName,password,enablement,person) -> UserRegistered(person.contact.emailAddress) |> Choice1Of2
    | ChangePassword (current,changeTo)                       -> UserPasswordChanged(user.userName) |> Choice1Of2
    | ChangeContactInformation contact                        -> ContactInformationChanged(contact) |> Choice1Of2
    | ChangeName name                                         -> PersonNameChanged(name) |> Choice1Of2
    | DefineEnablement enablement                             -> UserEnablementChanged(enablement) |> Choice1Of2

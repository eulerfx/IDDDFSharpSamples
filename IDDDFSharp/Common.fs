[<AutoOpen>]
module Common

open System

type DateTime = System.DateTime

type Guid = System.Guid

type TenantId = TenantId of string

type EmailAddress = EmailAddress of string

type Telephone = Telephone of string

type PostalAddress = {
    street : string;
    city : string;
    state : string;
    zip : string;
    countryCode : string;
};

type FullName = {
    firstName : string;
    lastName : string;
};


type Duration = Duration of DateTime * DateTime with 

    member x.IsAvailableOn(dt) =        
        if Duration.OpenEnded = x then true
        else 
            let now = dt
            let (Duration(startingOn,until)) = x            
            if now >= startingOn && now <= until then true
            else false

    member x.IsAvailable = x.IsAvailableOn(DateTime.Now)

    member x.StartingOn(startingOn) = 
        let (Duration(_,until)) = x
        Duration(startingOn, until) 

    member x.Until(until) = 
        let (Duration(startingOn,_)) = x
        Duration(startingOn, until)

    static member OpenEnded = Duration(DateTime.MinValue,DateTime.MinValue)

    static member create (startinOn,until) = Duration(startinOn,until)


//type EventMeta = {
//    version : int;
//    occurredOn : DateTime;
//    tenantId : string;
//    userName : string;
//}
//    with static member Null = { version = 0; occurredOn = DateTime.MinValue; tenantId = null; userName = null }    
            
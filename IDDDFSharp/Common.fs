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

type EventMeta = {
    version : int;
    occurredOn : DateTime;
    tenantId : string;
    userName : string;
}
    with static member Null = { version = 0; occurredOn = DateTime.MinValue; tenantId = null; userName = null }    
            
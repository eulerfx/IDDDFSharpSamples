module Teams


type Member = {
    tenantId : TenantId;
    userName : string;
    firstName : string;
    lastName : string;
    emailAddress : string;
    initiatedOn : DateTime;
}

type ProductOwnerId = ProductOwnerId of string
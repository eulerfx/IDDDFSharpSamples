module Collaborators

type Collaborator = {
    emailAddress : string;
    identity : string;
    name : string;
    collaboratorType : CollaboratorType; }

and CollaboratorType = Author | Creator | Moderator | Owner | Participant


//type Collaborator =
//    | Author of CollaboratorData
//    | Creator of CollaboratorData
//    | Moderator of CollaboratorData
//    | Owner of CollaboratorData
//    | Participant of CollaboratorData
//
//and CollaboratorData = {
//    emailAddress : string;
//    identity : string;
//    name : string; }


//type Collaborator = {
//    emailAddress : string;
//    identity : string;
//    name : string; }
//
//and Author = Author of Collaborator
//
//and Creator = Creator of Collaborator
//
//and Moderator = Moderator of Collaborator
//
//and Owner = Owner of Collaborator
//
//and Participant = Participant of Collaborator




type ICollaboratorService =
    abstract member GetCollaborator : Tenant * string * CollaboratorType -> Collaborator
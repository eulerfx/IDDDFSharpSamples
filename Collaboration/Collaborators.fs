module Collaborators

type Collaborator = {
    emailAddress : string;
    identity : string;
    name : string;
    collaboratorType : CollaboratorType; }

and CollaboratorType = Author | Creator | Moderator | Owner | Participant

type ICollaboratorService =
    abstract member GetCollaborator : Tenant * string * CollaboratorType -> Collaborator
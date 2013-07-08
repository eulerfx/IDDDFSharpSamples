module Projections

module Sql =

    open System.Data
    open System.Data.Common

    let connectionProvider (connectionString:string,providerName:string) = 
        let factory = providerName |> DbProviderFactories.GetFactory
        (fun () ->
            let conn = factory.CreateConnection() in
            conn.ConnectionString <- connectionString
            conn.Open()
            conn)

    type DataSource (connectionString,providerName) =
        member x.CreateOpenConnection = connectionProvider (connectionString,providerName)
    

    type SqlHelper (conn:unit -> DbConnection) =

        let exec action (query,args:obj list)  =
            use conn = conn ()
            use statement = conn.CreateCommand()
            statement.CommandText <- query
            for arg in args do
                let param = statement.CreateParameter()
                param.Value <- arg
                statement.Parameters.Add(param) |> ignore
            statement |> action

        member x.Exists = exec (fun c -> c.ExecuteScalar() <> null)

        member x.ExecuteUpdate = exec (fun c -> c.ExecuteNonQuery() |> ignore)
                
                                


type 'TEvent DispatchableEvent = {
    id : string;
    version : int;
    event : 'TEvent; }




module Calendars =

    open Calendars
    open Calendars.Calendar
    open Sql

    let project (sql:SqlHelper) (event:Event DispatchableEvent) = 

        let ids id = let ((Tenant tenantId),(CalendarId calendarId)) = id in tenantId,calendarId

        let update (cmd,args) = sql.ExecuteUpdate (cmd,args)
        
        let exists (cmd,args) = sql.Exists (cmd,args)

        let insertCalendarSharer (id:Tenant * CalendarId, sharer:CalendarSharer) =            
            let tenantId,calendarId = id |> ids
            if exists ("select id from tbl_vw_calendar_sharer where tenant_id = ? and calendar_id = ? and participant_identity = ?", [ tenantId; calendarId; sharer.participant.identity ]) then ()
            else
               update (
                    "insert into tbl_vw_calendar_sharer(id, calendar_id,participant_email_address, participant_identity, participant_name,tenant_id) values(?,?,?,?,?,?)",
                    [0;calendarId;sharer.participant.emailAddress;sharer.participant.identity;sharer.participant.name;tenantId])                  
                
        match event.event with
        | Created (id,name,desc,owner,sharedWith) ->            
            if exists ("select calendar_id from tbl_vw_calendar where tenant_id = ? and calendar_id = ?", []) then ()
            else
                let tenantId,calendarId = id |> ids
                update ("insert into tbl_vw_calendar(calendar_id, description, name, owner_email_address, owner_identity, owner_name, tenant_id) values (?,?,?,?,?,?,?)", 
                    [ calendarId; desc; name; owner.emailAddress; owner.identity; owner.name; tenantId; ])
                sharedWith |> Seq.iter (fun s -> insertCalendarSharer (id,s))                
        | DescriptionChanged (id,desc) ->
            let tenantId,calendarId = id |> ids
            update ("update tbl_vw_calendar set description=? where calendar_id = ?", [desc,calendarId])
        | Renamed (id,name) ->
            let tenantId,calendarId = id |> ids
            update ("update tbl_vw_calendar set name=? where calendar_id = ?", [name,calendarId])
        | CalendarShared (id,name,sharedWith) ->
            insertCalendarSharer (id,sharedWith)
        | CalendarUnshared (id,name,unsharedWith) ->
            let tenantId,calendarId = id |> ids
            update ("delete from tbl_vw_calendar_sharer where tenant_id=? and calendar_id=? and participant_identity=?",
                [tenantId;calendarId;unsharedWith.participant.identity])          
            
                

    

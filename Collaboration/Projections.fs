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
                
                                


type DispatchableEvent<'TEvent> = {
    id : string;
    version : int;
    event : 'TEvent; }


module Calendar =

    open Calendars
    open Calendars.Calendar
    open Sql

    let project (sql:SqlHelper) (event:DispatchableEvent<Event>) = 

        let ids id = let ((Tenant tenantId),(CalendarId calendarId)) = id in tenantId,calendarId

        let update,exists = sql.ExecuteUpdate,sql.Exists

        let insertCalendarSharer (id:Tenant * CalendarId, sharer:CalendarSharer) =            
            let tenantId,calendarId = id |> ids
            if exists ("select id from tbl_vw_calendar_sharer where tenant_id = ? and calendar_id = ? and participant_identity = ?", 
                       [tenantId;calendarId;sharer.participant.identity]) then ()
            else
               update ("insert into tbl_vw_calendar_sharer(id, calendar_id,participant_email_address, participant_identity, participant_name,tenant_id) values(?,?,?,?,?,?)",
                       [0;calendarId;sharer.participant.emailAddress;sharer.participant.identity;sharer.participant.name;tenantId])                  
                        
        match event.event with
        
        | Created (id,name,desc,owner,sharedWith) ->            
            let tenantId,calendarId = id |> ids
            if exists ("select calendar_id from tbl_vw_calendar where tenant_id = ? and calendar_id = ?", [tenantId;calendarId]) then ()
            else                
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
            
                

module CalendarEntry =
    
    open Calendars
    open Calendars.CalendarEntry
    open Collaborators
    open Sql
    open System

    let project (sql:SqlHelper) (event:DispatchableEvent<Event>) = 

        let ids id = let (Tenant tenantId),(CalendarId calendarId),(CalendarEntryId calendarEntryId) = id in tenantId,calendarId,calendarEntryId

        let update,exists = sql.ExecuteUpdate,sql.Exists
    
        let insertInvitee id (participant:Collaborator) =
            let tenantId,calendarId,calendarEntryId = id |> ids
            if exists ("select id from tbl_vw_calendar_entry_invitee where tenant_id = ? and calendar_entry_id = ? and participant_identity = ?",[tenantId;calendarEntryId;participant.identity]) then ()
            else update ("insert into tbl_vw_calendar_entry_invitee(id, calendar_entry_id, participant_email_address, participant_identity, participant_name, tenant_id) values(?,?,?,?,?,?)",
                         [0;calendarEntryId;participant.emailAddress;participant.identity;participant.name;tenantId])                                    


        match event.event with
        
        | Scheduled (id,desc,location,owner,timeSpan,repetition,alarm,invitees) ->            
            let tenantId,calendarId,calendarEntryId = id |> ids
            if exists ("select calendar_entry_id from tbl_vw_calendar_entry where tenant_id = ? and calendar_entry_id = ?", [tenantId;calendarEntryId]) then ()
            else            
                
                update ("insert into tbl_vw_calendar_entry(calendar_entry_id, alarm_alarm_units, alarm_alarm_units_type, calendar_id, description, location,owner_email_address, owner_identity, owner_name, repetition_ends, repetition_type, tenant_id, time_span_begins, time_span_ends ) values(?,?,?,?,?,?,?,?,?,?,?,?,?,?)", 
                        [calendarEntryId; alarm.alarmUnits; alarm.alarmUnitsType.ToString(); calendarId; ""; location;
                            owner.emailAddress; owner.identity;owner.name; defaultArg repetition.ends DateTime.MaxValue; tenantId; timeSpan.begins; timeSpan.ends; ])

                invitees |> Seq.iter (fun s -> insertInvitee id s)                
        
        | Rescheduled (id,timeSpan,repetition,alarm) ->
            let tenantId,calendarId,calendarEntryId = id |> ids
            update ("update tbl_vw_calendar_entry set alarm_alarm_units = ?, alarm_alarm_units_type = ?, repetition_ends = ?, repetition_type = ?, time_span_begins = ?, time_span_ends = ?  where tenant_id = ? and calendar_entry_id = ?", 
                    [ alarm.alarmUnits; alarm.alarmUnitsType.ToString(); repetition.ends; repetition.repeatType.ToString(); 
                      timeSpan.begins; timeSpan.ends; tenantId; calendarEntryId; ])
        
        | Relocated (id,location)->
            let tenantId,calendarId,calendarEntryId = id |> ids
            update ("update tbl_vw_calendar_entry set location=?  where calendar_entry_id = ?", [ location; calendarEntryId ])
        
        | ParticipantInvited (id,participant) ->
            insertInvitee id participant

        | ParticipantUninvited (id,participant) ->
            let tenantId,calendarId,calendarEntryId = id |> ids
            update ("delete from tbl_vw_calendar_entry_invitee where tenant_id = ? and calendar_entry_id = ? and participant_identity = ?",
                    [tenantId; calendarEntryId; participant.identity])     

        | DescriptionChanged (id,desc) ->
            let tenantId,calendarId,calendarEntryId = id |> ids
            update ("update tbl_vw_calendar_entry set description=? where calendar_entry_id = ?",
                    [ desc; calendarEntryId ])
            
            

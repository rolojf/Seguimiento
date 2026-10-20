module BoardStore exposing (apply, timestamp, validDate, validOffset, validTimestamp, validateOpportunity, validatePending, validateProcess)

import Dict exposing (Dict)
import Domain exposing (..)
import Time
import Url


titleLimit : Int
titleLimit =
    90


textLimit : Int
textLimit =
    2000


contextLimit : Int
contextLimit =
    240


taskNotesLimit : Int
taskNotesLimit =
    10000


offsetLimit : Float
offsetLimit =
    0.32


type alias Offset =
    { dx : Float, dy : Float }


cornerOffset : Float
cornerOffset =
    0.3


initialVersion : Int
initialVersion =
    1


templateVersion : Int
templateVersion =
    1



-- Every accepted command advances one board revision; rejected commands change nothing.


apply : User -> String -> Change -> Board -> Result String Board
apply actor now change board =
    if not (validTimestamp now) then
        Err "La fecha del servidor no es válida."

    else
        changeBoard actor now change board
            |> Result.andThen (syncLinkedPending now board)
            |> Result.map (\updated -> { updated | revision = board.revision + 1 })


changeBoard : User -> String -> Change -> Board -> Result String Board
changeBoard actor now change board =
    case change of
        SaveOpportunity target fields ->
            validateOpportunity fields
                |> Result.andThen (\_ -> saveOpportunity actor now target fields board)

        MoveOpportunity reference placement ->
            if validOffset placement.dx && validOffset placement.dy then
                changeOpportunity now
                    reference
                    (\item ->
                        let
                            fields =
                                item.fields
                        in
                        { item | fields = { fields | x = placement.x, y = placement.y }, dx = placement.dx, dy = placement.dy }
                    )
                    board

            else
                Err "El desplazamiento debe estar entre -0.32 y 0.32."

        ValidateOpportunity reference ->
            validateProposal actor now reference board

        ConfirmOpportunity reference ->
            confirmOpportunity actor now reference board

        RemoveOpportunity reference ->
            changeOpportunity now reference (\item -> { item | status = DiscardedOpportunity, review = Reviewed }) board

        RestoreOpportunity reference ->
            current reference board.opportunities
                |> Result.andThen
                    (\item ->
                        if opportunityDiscarded item then
                            Ok { board | opportunities = Dict.insert item.id (touch now { item | status = ActiveOpportunity NoCompletionProposal }) board.opportunities }

                        else
                            Err "Solo se pueden restaurar oportunidades descartadas."
                    )

        SuggestCompletion reference proposal ->
            if actor == UserTwo then
                changeOpportunity now reference (\item -> { item | status = ActiveOpportunity proposal }) board

            else
                Err "Solo Dot puede sugerir candidatos a completado."

        CompleteOpportunity reference ->
            if actor == UserOne then
                changeOpportunity now reference (\item -> { item | status = CompletedOpportunity, review = Reviewed }) board

            else
                Err "Solo Rolo puede completar oportunidades."

        PurgeOpportunity reference ->
            purgeOpportunity actor reference board

        SavePending target fields move completionEdit ->
            validatePending fields
                |> Result.andThen (\_ -> savePending actor now target fields move completionEdit board)

        MovePending reference shift ->
            movePending now reference shift board

        TogglePending reference ->
            togglePending actor now reference board

        RejectPendingCompletion reference ->
            rejectPendingCompletion actor now reference board

        ConfirmPending reference ->
            confirmPending actor reference board

        RemovePending reference ->
            discardPending now reference board

        RestorePending reference ->
            restorePending actor now reference board

        PurgePending reference ->
            purgePending reference board

        PurgeCompleted owner reference ->
            purgeCompleted owner reference board

        PurgeSeparator reference ->
            purgeSeparator reference board

        CreateProcess fields ->
            validateProcess fields
                |> Result.map (\_ -> createProcess now fields board)

        SendToPending reference taskId ->
            sendToPending actor now reference taskId board

        WithdrawPending reference taskId ->
            withdrawPending now reference taskId board

        ChangeTask reference taskId changeTask ->
            changeProcess actor now reference taskId changeTask board

        SaveTaskNotes reference taskId notes ->
            saveTaskNotes now reference taskId notes board

        CompleteProcess reference ->
            completeProcess actor now reference board

        PurgeProcess reference ->
            purgeProcess actor reference board



-- Synchronize only status transitions, never deletions: purging a pending preserves its result.


syncLinkedPending : String -> Board -> Board -> Result String Board
syncLinkedPending now previous updated =
    Dict.values updated.pending
        |> List.filter
            (\item ->
                item.origin
                    /= Standalone
                    && not item.deleted
                    && (case Dict.get item.id previous.pending of
                            Nothing ->
                                True

                            Just old ->
                                old.deleted || pendingStatus old /= pendingStatus item
                       )
            )
        |> List.foldl
            (\item result -> Result.andThen (syncPendingTask item) result)
            (Ok updated)
        |> Result.map
            (\synced ->
                { synced
                    | processes =
                        Dict.map
                            (\id process ->
                                if Maybe.map .states (Dict.get id updated.processes) /= Just process.states then
                                    touch now process

                                else
                                    process
                            )
                            synced.processes
                }
            )


pendingStatus : Pending -> Maybe PendingStatus
pendingStatus item =
    case item.fields.content of
        PendingTask _ status ->
            Just status

        Separator _ ->
            Nothing


syncPendingTask : Pending -> Board -> Result String Board
syncPendingTask item board =
    linkedTask item board
        |> Result.map
            (\( process, state ) ->
                let
                    completion =
                        if pendingIsComplete item.fields.content then
                            Done

                        else
                            Todo

                    updated =
                        setOwnerState (OwnerState item.fields.owner completion Reviewed) state
                in
                case item.origin of
                    ProcessTask _ taskId ->
                        { board | processes = Dict.insert process.id { process | states = Dict.insert taskId updated process.states } board.processes }

                    Standalone ->
                        board
            )


linkedTask : Pending -> Board -> Result String ( Process, TaskState )
linkedTask item board =
    case item.origin of
        Standalone ->
            Err "El pendiente no está vinculado a un proceso."

        ProcessTask processId taskId ->
            case Dict.get processId board.processes of
                Nothing ->
                    Err "El proceso vinculado ya no existe."

                Just process ->
                    activeProcess (ref process) board
                        |> Result.andThen
                            (\_ ->
                                case ( List.filter (\task -> task.id == taskId) (tasks process.fields.kind), Dict.get taskId process.states ) of
                                    ( [ task ], Just state ) ->
                                        assignedOwner task item.fields.owner
                                            |> Result.andThen
                                                (\_ ->
                                                    if state.applicability == Applicable then
                                                        Ok ( process, state )

                                                    else
                                                        Err "Restaura la actividad cancelada antes de recuperar o reabrir sus pendientes."
                                                )

                                    _ ->
                                        Err "La actividad vinculada no pertenece al proceso."
                            )


pendingProcess : Pending -> Maybe String
pendingProcess item =
    case item.origin of
        Standalone ->
            Nothing

        ProcessTask processId _ ->
            Just processId


saveOpportunity : User -> String -> Target -> OpportunityFields -> Board -> Result String Board
saveOpportunity actor now target fields board =
    case target of
        Existing reference ->
            changeOpportunity now
                reference
                (\item ->
                    let
                        offset =
                            if item.fields.x == fields.x && item.fields.y == fields.y then
                                Offset item.dx item.dy

                            else
                                nextOffset fields (Just item.id) board
                    in
                    { item | fields = fields, dx = offset.dx, dy = offset.dy }
                )
                board

        New ->
            let
                ( id, nextId ) =
                    freshId board

                offset =
                    nextOffset fields Nothing board

                item =
                    Opportunity id fields offset.dx offset.dy (ActiveOpportunity NoCompletionProposal) initialVersion now now (NeedsReview (otherUser actor))
            in
            Ok { board | opportunities = Dict.insert id item board.opportunities, nextId = nextId }


savePending : User -> String -> Target -> PendingFields -> PendingMove -> CompletionEdit -> Board -> Result String Board
savePending actor now target fields move completionEdit board =
    checkPendingMove move board
        |> Result.andThen
            (\_ ->
                case target of
                    Existing reference ->
                        active reference board.pending
                            |> Result.andThen (\item -> saveExistingPending actor now fields move completionEdit item board)

                    New ->
                        applyCompletionEdit actor completionEdit (withApproval actor fields)
                            |> Result.andThen
                                (\updatedFields ->
                                    checkCompletedMove move updatedFields.content
                                        |> Result.map
                                            (\_ ->
                                                let
                                                    ( id, nextId ) =
                                                        freshId board

                                                    item =
                                                        Pending id updatedFields False initialVersion now now (List.length (orderedPending updatedFields.owner board)) Standalone

                                                    saved =
                                                        { board | pending = Dict.insert id item board.pending, nextId = nextId }
                                                in
                                                shiftIfRequested move item saved
                                            )
                                )
            )
        |> Result.map (advancePendingOrder board)


saveExistingPending : User -> String -> PendingFields -> PendingMove -> CompletionEdit -> Pending -> Board -> Result String Board
saveExistingPending actor now fields move completionEdit item board =
    if item.origin /= Standalone && fields.owner /= item.fields.owner then
        Err "El responsable de un pendiente vinculado pertenece al proceso y no se puede cambiar."

    else if item.origin /= Standalone && pendingCompletion fields.content == Nothing then
        Err "Un pendiente vinculado a un proceso no se puede convertir en separador."

    else if pendingCompletion item.fields.content == Just Done && fields.owner /= item.fields.owner then
        Err "Reabre el pendiente antes de cambiar su responsable."

    else if pendingCompletion item.fields.content == Just Done && pendingCompletion fields.content == Nothing then
        Err "Reabre el pendiente antes de convertirlo en separador."

    else
        applyCompletionEdit actor completionEdit (editPendingFields actor fields item)
            |> Result.andThen
                (\updatedFields ->
                    checkCompletedMove move updatedFields.content
                        |> Result.map
                            (\_ ->
                                let
                                    edited =
                                        { item
                                            | fields = updatedFields
                                            , version = item.version + 1
                                            , updatedAt =
                                                if pendingCompletion item.fields.content == Just Done && pendingCompletion updatedFields.content == Just Done then
                                                    item.updatedAt

                                                else
                                                    now
                                            , position =
                                                if item.fields.owner /= updatedFields.owner || (pendingIsComplete item.fields.content && not (pendingIsComplete updatedFields.content)) then
                                                    List.length (orderedPending updatedFields.owner board)

                                                else
                                                    item.position
                                        }

                                    saved =
                                        { board | pending = Dict.insert item.id edited board.pending }
                                            |> normalizePending item.fields.owner
                                            |> normalizePending updatedFields.owner
                                in
                                shiftIfRequested move edited saved
                            )
                )


checkCompletedMove : PendingMove -> PendingContent -> Result String ()
checkCompletedMove move content =
    case move of
        ShiftPosition shift ->
            if pendingIsComplete content && shift.steps /= 0 then
                Err "Reabre el pendiente completado antes de moverlo."

            else
                Ok ()

        KeepPosition ->
            Ok ()



-- The editor sets a desired value; saving Done must never acknowledge a review.


applyCompletionEdit : User -> CompletionEdit -> PendingFields -> Result String PendingFields
applyCompletionEdit actor completionEdit fields =
    case ( completionEdit, fields.content ) of
        ( KeepCompletion, _ ) ->
            Ok fields

        ( ChangeCompletion _, Separator _ ) ->
            Err "Un separador no tiene estado de completado."

        ( ChangeCompletion desired, PendingTask title status ) ->
            editCompletionStatus actor fields.owner desired status
                |> Result.map (\updated -> { fields | content = PendingTask title updated })


editCompletionStatus : User -> Owner -> Completion -> PendingStatus -> Result String PendingStatus
editCompletionStatus actor owner desired status =
    case desired of
        Todo ->
            Ok (reopenPendingStatus actor owner status)

        Done ->
            case status of
                AwaitingApproval _ ->
                    Err "Primero debe aprobarse el pendiente antes de completarlo."

                Approved ->
                    Ok (completedStatus actor owner)

                _ ->
                    Ok status



-- Approval is assigned by the backend; submitted fields cannot grant it.


initialApproval : User -> Owner -> PendingStatus
initialApproval actor owner =
    case ( actor, owner ) of
        ( UserOne, Rolando ) ->
            Approved

        ( UserOne, Dot ) ->
            AwaitingApproval UserTwo

        ( UserTwo, Rolando ) ->
            AwaitingApproval UserOne

        ( UserTwo, Dot ) ->
            AwaitingApproval UserOne

        ( _, Others ) ->
            AwaitingApproval (otherUser actor)


withApproval : User -> PendingFields -> PendingFields
withApproval actor fields =
    case fields.content of
        PendingTask title _ ->
            { fields | content = PendingTask title (initialApproval actor fields.owner) }

        Separator _ ->
            fields


editPendingFields : User -> PendingFields -> Pending -> PendingFields
editPendingFields actor fields item =
    case ( fields.content, item.fields.content ) of
        ( PendingTask title _, PendingTask _ status ) ->
            if fields.owner == item.fields.owner then
                { fields | content = PendingTask title status }

            else
                withApproval actor fields

        _ ->
            withApproval actor fields


togglePending : User -> String -> Ref -> Board -> Result String Board
togglePending actor now reference board =
    active reference board.pending
        |> Result.andThen
            (\item ->
                case item.fields.content of
                    Separator _ ->
                        Err "Un separador no se puede marcar como completado."

                    PendingTask title status ->
                        togglePendingStatus actor item.fields.owner status
                            |> Result.map (\updated -> storePendingStatus now item title updated board)
            )


rejectPendingCompletion : User -> String -> Ref -> Board -> Result String Board
rejectPendingCompletion actor now reference board =
    active reference board.pending
        |> Result.andThen
            (\item ->
                case item.fields.content of
                    PendingTask title (CompletedBy author (NeedsReview reviewer)) ->
                        if reviewer == actor && actor /= author then
                            Ok (storePendingStatus now item title (AwaitingApproval author) board)

                        else
                            Err "Solo el otro usuario pendiente de revisión puede rechazar este completado."

                    _ ->
                        Err "El pendiente ya no tiene un completado por revisar."
            )


togglePendingStatus : User -> Owner -> PendingStatus -> Result String PendingStatus
togglePendingStatus actor owner status =
    case status of
        AwaitingApproval _ ->
            Err "Primero debe aprobarse el pendiente antes de completarlo."

        Approved ->
            Ok (completedStatus actor owner)

        CompletedBy author review ->
            if review == NeedsReview actor then
                Ok (CompletedBy author Reviewed)

            else
                Ok (reopenPendingStatus actor owner status)

        LegacyCompleted ->
            Ok (reopenPendingStatus actor owner status)


completedStatus : User -> Owner -> PendingStatus
completedStatus actor owner =
    CompletedBy actor
        (if actor == UserOne && owner == Rolando then
            Reviewed

         else
            NeedsReview (otherUser actor)
        )


reopenPendingStatus : User -> Owner -> PendingStatus -> PendingStatus
reopenPendingStatus actor owner status =
    case status of
        CompletedBy author _ ->
            if actor == author then
                initialApproval actor owner

            else
                AwaitingApproval author

        LegacyCompleted ->
            initialApproval actor owner

        _ ->
            status


storePendingStatus : String -> Pending -> String -> PendingStatus -> Board -> Board
storePendingStatus now item title status board =
    let
        fields =
            item.fields

        content =
            PendingTask title status

        updated =
            touch now
                { item
                    | fields = { fields | content = content }
                    , position =
                        if pendingIsComplete fields.content && not (pendingIsComplete content) then
                            List.length (orderedPending fields.owner board)

                        else
                            item.position
                }
    in
    { board | pending = Dict.insert item.id updated board.pending }
        |> normalizePending fields.owner
        |> advancePendingOrder board


discardPending : String -> Ref -> Board -> Result String Board
discardPending now reference board =
    active reference board.pending
        |> Result.andThen
            (\item ->
                case item.fields.content of
                    Separator _ ->
                        Err "Los separadores se borran directamente; no se descartan."

                    PendingTask _ _ ->
                        Ok
                            ({ board | pending = Dict.insert item.id (touch now { item | deleted = True }) board.pending }
                                |> normalizePending item.fields.owner
                                |> advancePendingOrder board
                            )
            )


restorePending : User -> String -> Ref -> Board -> Result String Board
restorePending actor now reference board =
    current reference board.pending
        |> Result.andThen
            (\item ->
                if item.deleted then
                    let
                        restored =
                            touch now
                                { item
                                    | deleted = False
                                    , fields =
                                        if item.origin /= Standalone && not (pendingIsComplete item.fields.content) then
                                            withApproval actor item.fields

                                        else
                                            item.fields
                                    , position =
                                        if pendingIsComplete item.fields.content then
                                            item.position

                                        else
                                            List.length (orderedPending item.fields.owner board)
                                }
                    in
                    Ok
                        ({ board | pending = Dict.insert item.id restored board.pending }
                            |> advancePendingOrder board
                        )

                else
                    Err "El pendiente ya está activo."
            )


purgePending : Ref -> Board -> Result String Board
purgePending reference board =
    current reference board.pending
        |> Result.andThen
            (\item ->
                if item.deleted then
                    Ok (removePendingItem item board)

                else
                    Err "Descarta el pendiente antes de borrarlo definitivamente."
            )


purgeCompleted : Owner -> Ref -> Board -> Result String Board
purgeCompleted owner reference board =
    active reference board.pending
        |> Result.andThen
            (\item ->
                if item.fields.owner /= owner || not (pendingIsComplete item.fields.content) then
                    Err "El pendiente no pertenece a los completados de este responsable."

                else if (completedPending owner board |> List.reverse |> List.head |> Maybe.map .id) /= Just item.id then
                    Err "Solo se puede borrar el último pendiente de la lista de completados actual."

                else
                    Ok (removePendingItem item board)
            )


purgeSeparator : Ref -> Board -> Result String Board
purgeSeparator reference board =
    current reference board.pending
        |> Result.andThen
            (\item ->
                case item.fields.content of
                    Separator _ ->
                        Ok (removePendingItem item board)

                    PendingTask _ _ ->
                        Err "Esta acción solo puede borrar separadores."
            )


removePendingItem : Pending -> Board -> Board
removePendingItem item board =
    -- Keep nextId unchanged: deleting a record never makes its identifier reusable.
    { board | pending = Dict.remove item.id board.pending, pendingOrderRevision = board.pendingOrderRevision + 1 }
        |> normalizePending item.fields.owner


movePending : String -> Ref -> PendingShift -> Board -> Result String Board
movePending now reference shift board =
    active reference board.pending
        |> Result.andThen
            (\item ->
                if pendingIsComplete item.fields.content then
                    Err "Reabre el pendiente completado antes de moverlo."

                else
                    checkPendingMove (ShiftPosition shift) board
                        |> Result.map
                            (\_ ->
                                let
                                    moved =
                                        shiftPending item shift.steps board |> advancePendingOrder board
                                in
                                if moved.pendingOrderRevision == board.pendingOrderRevision then
                                    moved

                                else
                                    { moved | pending = Dict.update item.id (Maybe.map (touch now)) moved.pending }
                            )
            )


checkPendingMove : PendingMove -> Board -> Result String ()
checkPendingMove move board =
    case move of
        KeepPosition ->
            Ok ()

        ShiftPosition shift ->
            if shift.revision == board.pendingOrderRevision then
                Ok ()

            else
                Err "El orden de pendientes cambió. Revisa la lista antes de volver a mover el elemento."


shiftIfRequested : PendingMove -> Pending -> Board -> Board
shiftIfRequested move item board =
    case move of
        KeepPosition ->
            board

        ShiftPosition shift ->
            shiftPending item shift.steps board



-- Move IDs only; reindexing must never overwrite a neighbour's content or status.


shiftPending : Pending -> Int -> Board -> Board
shiftPending item steps board =
    let
        ids =
            orderedPending item.fields.owner board |> List.map .id

        position =
            ids |> List.indexedMap Tuple.pair |> List.filter (\( _, id ) -> id == item.id) |> List.head |> Maybe.map Tuple.first
    in
    case position of
        Nothing ->
            board

        Just previous ->
            let
                destination =
                    clamp 0 (List.length ids - 1) (previous - steps)

                remaining =
                    List.filter ((/=) item.id) ids
            in
            if destination == previous then
                board

            else
                setPendingOrder (List.take destination remaining ++ [ item.id ] ++ List.drop destination remaining) board


normalizePending : Owner -> Board -> Board
normalizePending owner board =
    setPendingOrder (orderedPending owner board |> List.map .id) board


setPendingOrder : List String -> Board -> Board
setPendingOrder ids board =
    { board
        | pending =
            ids
                |> List.indexedMap Tuple.pair
                |> List.foldl
                    (\( position, id ) items -> Dict.update id (Maybe.map (\item -> { item | position = position })) items)
                    board.pending
    }


advancePendingOrder : Board -> Board -> Board
advancePendingOrder previous updated =
    let
        order board =
            List.map (\owner -> orderedPending owner board |> List.map .id) [ Rolando, Dot, Others ]
    in
    if order previous == order updated then
        updated

    else
        { updated | pendingOrderRevision = previous.pendingOrderRevision + 1 }


validateProposal : User -> String -> Ref -> Board -> Result String Board
validateProposal actor now reference board =
    activeOpportunity reference board
        |> Result.andThen
            (\item ->
                case item.review of
                    NeedsReview _ ->
                        confirmOpportunity actor now reference board

                    Reviewed ->
                        changeOpportunity now reference markValidated board
            )


confirmOpportunity : User -> String -> Ref -> Board -> Result String Board
confirmOpportunity actor now reference board =
    activeOpportunity reference board
        |> Result.andThen
            (\item ->
                requireReview actor item.review
                    |> Result.map
                        (\_ ->
                            { board | opportunities = Dict.insert item.id (touch now (markValidated item)) board.opportunities }
                        )
            )


markValidated : Opportunity -> Opportunity
markValidated item =
    let
        fields =
            item.fields
    in
    { item | fields = { fields | proposed = False }, review = Reviewed }


confirmPending : User -> Ref -> Board -> Result String Board
confirmPending actor reference board =
    active reference board.pending
        |> Result.andThen
            (\item ->
                case item.fields.content of
                    PendingTask title (AwaitingApproval reviewer) ->
                        if actor == reviewer then
                            let
                                fields =
                                    item.fields

                                approved =
                                    { item | fields = { fields | content = PendingTask title Approved }, version = item.version + 1 }
                            in
                            Ok { board | pending = Dict.insert item.id approved board.pending }

                        else
                            Err "La aprobación de este pendiente corresponde al otro usuario."

                    _ ->
                        Err "Este elemento no está esperando aprobación."
            )


requireReview : User -> Review -> Result String ()
requireReview actor review =
    if review == NeedsReview actor then
        Ok ()

    else
        Err "No hay una confirmación pendiente para este usuario."


createProcess : String -> ProcessFields -> Board -> Board
createProcess now fields board =
    let
        ( id, nextId ) =
            freshId board

        states =
            tasks fields.kind
                |> List.map (\task -> ( task.id, initialTaskState task ))
                |> Dict.fromList

        item =
            Process id { fields | client = String.trim fields.client } ActiveProcess states Open "" templateVersion initialVersion now now
    in
    { board | processes = Dict.insert id item board.processes, nextId = nextId }



-- Match the original chart's repeatable separation inside an occupied rating cell.


nextOffset : OpportunityFields -> Maybe String -> Board -> Offset
nextOffset fields excluding board =
    let
        occupied =
            Dict.values board.opportunities
                |> List.filter (\item -> opportunityActive item && item.fields.x == fields.x && item.fields.y == fields.y && Just item.id /= excluding)

        first =
            Offset (negate cornerOffset) (negate cornerOffset)

        candidates =
            [ Offset cornerOffset cornerOffset
            , Offset (negate cornerOffset) cornerOffset
            , Offset cornerOffset (negate cornerOffset)
            , Offset 0 (negate offsetLimit)
            , Offset 0 offsetLimit
            , Offset (negate offsetLimit) 0
            , Offset offsetLimit 0
            , Offset 0 0
            ]
    in
    if List.isEmpty occupied then
        Offset 0 0

    else
        List.foldl
            (\candidate best ->
                if nearestDistance occupied candidate > nearestDistance occupied best then
                    candidate

                else
                    best
            )
            first
            candidates


nearestDistance : List Opportunity -> Offset -> Float
nearestDistance occupied point =
    let
        distance item =
            (item.dx - point.dx) ^ 2 + (item.dy - point.dy) ^ 2
    in
    case occupied of
        [] ->
            0

        first :: rest ->
            List.foldl (\item closest -> min closest (distance item)) (distance first) rest


changeOpportunity : String -> Ref -> (Opportunity -> Opportunity) -> Board -> Result String Board
changeOpportunity now reference update board =
    activeOpportunity reference board
        |> Result.map
            (\item -> { board | opportunities = Dict.insert item.id (touch now (update item)) board.opportunities })


activeOpportunity : Ref -> Board -> Result String Opportunity
activeOpportunity reference board =
    current reference board.opportunities
        |> Result.andThen
            (\item ->
                if opportunityActive item then
                    Ok item

                else
                    Err "La oportunidad ya no está activa."
            )


purgeOpportunity : User -> Ref -> Board -> Result String Board
purgeOpportunity actor reference board =
    if actor /= UserOne then
        Err "Solo Rolo puede borrar oportunidades."

    else
        current reference board.opportunities
            |> Result.andThen
                (\item ->
                    if opportunityComplete item then
                        -- The retained sequence prevents a deleted identifier from being reused.
                        Ok { board | opportunities = Dict.remove item.id board.opportunities }

                    else
                        Err "Completa la oportunidad antes de borrarla definitivamente."
                )


assignedOwner : Task -> Owner -> Result String ()
assignedOwner task owner =
    if List.member owner (taskOwners task) then
        Ok ()

    else
        Err "El responsable no está asignado a esta actividad."


setOwnerState : OwnerState -> TaskState -> TaskState
setOwnerState part state =
    { state
        | completions =
            if List.any (\currentPart -> currentPart.owner == part.owner) state.completions then
                List.map
                    (\currentPart ->
                        if currentPart.owner == part.owner then
                            part

                        else
                            currentPart
                    )
                    state.completions

            else
                state.completions ++ [ part ]
    }


checkTaskLink : String -> String -> TaskChange -> Board -> Result String ()
checkTaskLink processId taskId change board =
    case change of
        SetCompletion owner _ ->
            if Dict.values board.pending |> List.any (\item -> item.origin == ProcessTask processId taskId && item.fields.owner == owner && not item.deleted) then
                Err "Gestiona el cumplimiento de este responsable desde Pendientes."

            else
                Ok ()

        _ ->
            Ok ()


sendToPending : User -> String -> Ref -> String -> Board -> Result String Board
sendToPending actor now reference taskId board =
    activeProcess reference board
        |> Result.andThen
            (\process ->
                case ( List.filter (\task -> task.id == taskId) (tasks process.fields.kind), Dict.get taskId process.states ) of
                    ( [ task ], Just state ) ->
                        if state.applicability == NotApplicable then
                            Err "Restaura la actividad antes de enviarla a Pendientes."

                        else
                            taskOwners task
                                |> List.filter (\owner -> (ownerState owner state).completion /= Done)
                                |> List.foldl (sendOwnerPending actor now process task) board
                                |> advancePendingOrder board
                                |> advanceLinkVersion now process board
                                |> Ok

                    _ ->
                        Err "El paso no pertenece a la plantilla del proceso."
            )


sendOwnerPending : User -> String -> Process -> Task -> Owner -> Board -> Board
sendOwnerPending actor now process task owner board =
    case linkedPending process.id task.id owner board of
        Just item ->
            if not item.deleted || pendingIsComplete item.fields.content then
                board

            else
                let
                    restored =
                        touch now
                            { item
                                | deleted = False
                                , fields = withApproval actor item.fields
                                , position = List.length (orderedPending owner board)
                            }
                in
                { board | pending = Dict.insert item.id restored board.pending }
                    |> normalizePending owner

        Nothing ->
            let
                ( id, nextId ) =
                    freshId board

                prefix =
                    String.left (titleLimit // 3) process.fields.client ++ " " ++ processLabel process.fields.kind ++ " "

                title =
                    prefix ++ String.left (titleLimit - String.length prefix) task.label

                fields =
                    PendingFields (PendingTask title (initialApproval actor owner)) owner

                item =
                    Pending id fields False initialVersion now now (List.length (orderedPending owner board)) (ProcessTask process.id task.id)
            in
            { board | pending = Dict.insert id item board.pending, nextId = nextId }
                |> normalizePending owner



-- Detach finalized copies without touching their completion date or the task's reviews.


withdrawPending : String -> Ref -> String -> Board -> Result String Board
withdrawPending now reference taskId board =
    activeProcess reference board
        |> Result.andThen
            (\process ->
                case ( List.filter (\task -> task.id == taskId) (tasks process.fields.kind), Dict.get taskId process.states ) of
                    ( [ _ ], Just _ ) ->
                        { board
                            | pending =
                                Dict.map
                                    (\_ item ->
                                        if item.origin == ProcessTask process.id taskId && pendingIsComplete item.fields.content then
                                            { item | origin = Standalone, version = item.version + 1 }

                                        else
                                            item
                                    )
                                    board.pending
                        }
                            |> discardTaskPending now process.id taskId
                            |> advanceLinkVersion now process board
                            |> Ok

                    _ ->
                        Err "El paso no pertenece a la plantilla del proceso."
            )



-- Link changes invalidate earlier send/withdraw intentions even when task states stay the same.


advanceLinkVersion : String -> Process -> Board -> Board -> Board
advanceLinkVersion now process previous updated =
    if previous.pending == updated.pending then
        updated

    else
        { updated | processes = Dict.insert process.id (touch now process) updated.processes }



-- Cancellation retains finalized work and discards every unfinished linked pending atomically.


discardTaskPending : String -> String -> String -> Board -> Board
discardTaskPending now processId taskId board =
    let
        discarded =
            { board
                | pending =
                    Dict.map
                        (\_ item ->
                            if item.origin == ProcessTask processId taskId && not item.deleted && not (pendingIsComplete item.fields.content) then
                                touch now { item | deleted = True }

                            else
                                item
                        )
                        board.pending
            }
    in
    List.foldl normalizePending discarded [ Rolando, Dot, Others ]
        |> advancePendingOrder board


changeProcess : User -> String -> Ref -> String -> TaskChange -> Board -> Result String Board
changeProcess actor now reference taskId change board =
    activeProcess reference board
        |> Result.andThen
            (\process ->
                case ( List.filter (\task -> task.id == taskId) (tasks process.fields.kind), Dict.get taskId process.states ) of
                    ( [ task ], Just state ) ->
                        checkTaskLink process.id taskId change board
                            |> Result.andThen (\_ -> updateTask actor task change state)
                            |> Result.map
                                (\updated ->
                                    let
                                        -- Recognizing an alert changes its version, not the business timestamp.
                                        updatedAt =
                                            if updated.applicability == state.applicability && List.map .completion updated.completions == List.map .completion state.completions then
                                                process.updatedAt

                                            else
                                                now

                                        changed =
                                            { process
                                                | states = Dict.insert taskId updated process.states
                                                , version = process.version + 1
                                                , updatedAt = updatedAt
                                            }

                                        saved =
                                            { board | processes = Dict.insert process.id changed board.processes }
                                    in
                                    if updated.applicability == NotApplicable then
                                        discardTaskPending now process.id taskId saved

                                    else
                                        saved
                                )

                    _ ->
                        Err "El paso no pertenece a la plantilla del proceso."
            )



-- Completed processes retain their task state, but reject every task mutation.


activeProcess : Ref -> Board -> Result String Process
activeProcess reference board =
    current reference board.processes
        |> Result.andThen
            (\process ->
                if process.status == ActiveProcess then
                    Ok process

                else
                    Err "El proceso está completado y no admite modificaciones."
            )


completeProcess : User -> String -> Ref -> Board -> Result String Board
completeProcess actor now reference board =
    if actor /= UserOne then
        Err "Solo Rolo puede completar un proceso."

    else
        activeProcess reference board
            |> Result.andThen
                (\process ->
                    if Dict.values board.pending |> List.any (\item -> pendingProcess item == Just process.id && not item.deleted && not (pendingIsComplete item.fields.content)) then
                        Err "Finaliza o descarta los pendientes vinculados antes de completar el proceso."

                    else
                        Ok { board | processes = Dict.insert process.id (touch now { process | status = CompletedProcess }) board.processes }
                )


purgeProcess : User -> Ref -> Board -> Result String Board
purgeProcess actor reference board =
    if actor /= UserOne then
        Err "Solo Rolo puede borrar un proceso."

    else
        current reference board.processes
            |> Result.andThen
                (\process ->
                    if process.status == CompletedProcess then
                        Ok
                            { board
                                | processes = Dict.remove process.id board.processes
                                , pending =
                                    Dict.map
                                        (\_ item ->
                                            if pendingProcess item == Just process.id then
                                                { item | origin = Standalone, version = item.version + 1 }

                                            else
                                                item
                                        )
                                        board.pending
                            }

                    else
                        Err "Completa el proceso antes de borrarlo definitivamente."
                )


saveTaskNotes : String -> Ref -> String -> String -> Board -> Result String Board
saveTaskNotes now reference taskId notes board =
    if String.length notes > taskNotesLimit then
        Err "Las notas del paso admiten hasta 10000 caracteres."

    else
        activeProcess reference board
            |> Result.andThen
                (\process ->
                    case ( List.filter (\task -> task.id == taskId) (tasks process.fields.kind), Dict.get taskId process.states ) of
                        ( [ _ ], Just state ) ->
                            let
                                updated =
                                    touch now { process | states = Dict.insert taskId { state | notes = String.trim notes } process.states }
                            in
                            Ok { board | processes = Dict.insert process.id updated board.processes }

                        _ ->
                            Err "El paso no pertenece a la plantilla del proceso."
                )



-- Evidence and personal closure remain independent of task completion.


updateTask : User -> Task -> TaskChange -> TaskState -> Result String TaskState
updateTask actor task change state =
    case change of
        SetCompletion owner completion ->
            assignedOwner task owner
                |> Result.andThen (\_ -> canChange actor (ownerState owner state).review)
                |> Result.andThen
                    (\_ ->
                        if state.applicability == NotApplicable then
                            Err "Restaura el paso antes de cambiar su confirmación."

                        else if (ownerState owner state).completion == completion then
                            Ok state

                        else
                            Ok (setOwnerState (OwnerState owner completion (NeedsReview (otherUser actor))) state)
                    )

        ExcludeTask ->
            canChange actor state.applicabilityReview
                |> Result.andThen
                    (\_ ->
                        if not task.allowNotApplicable then
                            Err "Este paso no permite marcar No aplica."

                        else if state.applicability == NotApplicable then
                            Ok state

                        else
                            Ok
                                { state
                                    | applicability = NotApplicable
                                    , applicabilityReview = NeedsReview (otherUser actor)
                                }
                    )

        RestoreTask ->
            canChange actor state.applicabilityReview
                |> Result.map
                    (\_ ->
                        if state.applicability == Applicable then
                            state

                        else
                            { state
                                | applicability = Applicable
                                , applicabilityReview = NeedsReview (otherUser actor)
                            }
                    )

        Acknowledge checkbox ->
            case checkbox of
                CompletionBox owner ->
                    assignedOwner task owner
                        |> Result.andThen (\_ -> acknowledge actor checkbox state)

                ApplicabilityBox ->
                    acknowledge actor checkbox state


canChange : User -> Review -> Result String ()
canChange actor review =
    if review == NeedsReview actor then
        Err "Primero reconoce el cambio del otro usuario en esta casilla."

    else
        Ok ()



-- Each control has its own recipient; acknowledgement never notifies the author.


acknowledge : User -> Checkbox -> TaskState -> Result String TaskState
acknowledge actor checkbox state =
    let
        review =
            case checkbox of
                ApplicabilityBox ->
                    state.applicabilityReview

                CompletionBox owner ->
                    (ownerState owner state).review
    in
    if review /= NeedsReview actor then
        Err "Esta casilla no tiene un cambio pendiente de tu reconocimiento."

    else
        case checkbox of
            ApplicabilityBox ->
                Ok { state | applicabilityReview = Reviewed }

            CompletionBox owner ->
                let
                    part =
                        ownerState owner state
                in
                Ok (setOwnerState { part | review = Reviewed } state)


otherUser : User -> User
otherUser user =
    case user of
        UserOne ->
            UserTwo

        UserTwo ->
            UserOne


current : Ref -> Dict String { a | id : String, version : Int } -> Result String { a | id : String, version : Int }
current reference items =
    case Dict.get reference.id items of
        Nothing ->
            Err "El registro ya no está disponible."

        Just item ->
            if item.version == reference.version then
                Ok item

            else
                Err "El registro cambió en otro dispositivo. Revisa la versión actual antes de aplicar tu cambio."


active : Ref -> Dict String { a | id : String, version : Int, deleted : Bool } -> Result String { a | id : String, version : Int, deleted : Bool }
active reference items =
    current reference items
        |> Result.andThen
            (\item ->
                if item.deleted then
                    Err "El registro está eliminado."

                else
                    Ok item
            )


touch : String -> { a | version : Int, updatedAt : String } -> { a | version : Int, updatedAt : String }
touch now item =
    { item | version = item.version + 1, updatedAt = now }



-- One sequence covers all three tables, including imported and deleted records.


freshId : Board -> ( String, Int )
freshId board =
    seekId board (max 1 board.nextId)


seekId : Board -> Int -> ( String, Int )
seekId board candidate =
    let
        id =
            "elm-" ++ String.fromInt candidate
    in
    if Dict.member id board.opportunities || Dict.member id board.pending || Dict.member id board.processes then
        seekId board (candidate + 1)

    else
        ( id, candidate + 1 )


validateOpportunity : OpportunityFields -> Result String ()
validateOpportunity fields =
    if not (validTitle fields.title) then
        Err "El título es obligatorio y admite hasta 90 caracteres."

    else if String.length fields.notes > textLimit || String.length fields.source > textLimit then
        Err "Las notas y la fuente admiten hasta 2000 caracteres cada una."

    else if not (validSource fields.source) then
        Err "La fuente debe ser una dirección HTTP o HTTPS."

    else
        Ok ()


validatePending : PendingFields -> Result String ()
validatePending fields =
    case fields.content of
        PendingTask title _ ->
            if validTitle title then
                Ok ()

            else
                Err "El pendiente es obligatorio y admite hasta 90 caracteres."

        Separator title ->
            if String.length title <= titleLimit then
                Ok ()

            else
                Err "El separador admite hasta 90 caracteres."


validateProcess : ProcessFields -> Result String ()
validateProcess fields =
    if not (validTitle fields.client) then
        Err "El cliente es obligatorio y admite hasta 90 caracteres."

    else if not (validDate fields.startDate) then
        Err "La fecha de inicio debe ser una fecha real con formato AAAA-MM-DD."

    else if String.length fields.context > contextLimit then
        Err "El contexto admite hasta 240 caracteres."

    else
        Ok ()


validTitle : String -> Bool
validTitle value =
    not (String.isEmpty (String.trim value)) && String.length value <= titleLimit


validSource : String -> Bool
validSource value =
    String.isEmpty value
        || ((String.startsWith "https://" value || String.startsWith "http://" value)
                && (Url.fromString value |> Maybe.map (\url -> not (String.isEmpty url.host)) |> Maybe.withDefault False)
           )


validOffset : Float -> Bool
validOffset value =
    not (isNaN value || isInfinite value) && abs value <= offsetLimit


validDate : String -> Bool
validDate value =
    case String.split "-" value of
        [ yearText, monthText, dayText ] ->
            case ( String.toInt yearText, String.toInt monthText, String.toInt dayText ) of
                ( Just year, Just month, Just day ) ->
                    value
                        == padded 4 year
                        ++ "-"
                        ++ padded 2 month
                        ++ "-"
                        ++ padded 2 day
                        && year
                        >= 0
                        && year
                        <= 9999
                        && month
                        >= 1
                        && month
                        <= 12
                        && day
                        >= 1
                        && day
                        <= daysInMonth year month

                _ ->
                    False

        _ ->
            False


daysInMonth : Int -> Int -> Int
daysInMonth year month =
    if month == 2 then
        if modBy 400 year == 0 || (modBy 4 year == 0 && modBy 100 year /= 0) then
            29

        else
            28

    else if List.member month [ 4, 6, 9, 11 ] then
        30

    else
        31


validTimestamp : String -> Bool
validTimestamp value =
    String.length value
        == 24
        && validDate (String.left 10 value)
        && String.slice 10 11 value
        == "T"
        && String.slice 13 14 value
        == ":"
        && String.slice 16 17 value
        == ":"
        && String.slice 19 20 value
        == "."
        && String.right 1 value
        == "Z"
        && timePart 23 (String.slice 11 13 value)
        && timePart 59 (String.slice 14 16 value)
        && timePart 59 (String.slice 17 19 value)
        && timePart 999 (String.slice 20 23 value)


timePart : Int -> String -> Bool
timePart maximum value =
    case String.toInt value of
        Just number ->
            number >= 0 && number <= maximum && value == padded (String.length value) number

        Nothing ->
            False


timestamp : Time.Posix -> String
timestamp value =
    padded 4 (Time.toYear Time.utc value)
        ++ "-"
        ++ padded 2 (monthNumber (Time.toMonth Time.utc value))
        ++ "-"
        ++ padded 2 (Time.toDay Time.utc value)
        ++ "T"
        ++ padded 2 (Time.toHour Time.utc value)
        ++ ":"
        ++ padded 2 (Time.toMinute Time.utc value)
        ++ ":"
        ++ padded 2 (Time.toSecond Time.utc value)
        ++ "."
        ++ padded 3 (modBy 1000 (Time.posixToMillis value))
        ++ "Z"


padded : Int -> Int -> String
padded width value =
    String.padLeft width '0' (String.fromInt value)


monthNumber : Time.Month -> Int
monthNumber month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12

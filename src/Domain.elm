module Domain exposing (..)

import Dict exposing (Dict)


type User
    = UserOne
    | UserTwo


type Review
    = Reviewed
    | NeedsReview User


type Checkbox
    = ApplicabilityBox
    | CompletionBox Owner


type Category
    = Ventex
    | Contractor


type Kind
    = OpportunityKind
    | Responsibility


type Owner
    = Rolando
    | Dot
    | Others


type Completion
    = Todo
    | Done


type Rating
    = One
    | Two
    | Three
    | Four
    | Five


type ProcessKind
    = NewOti
    | Deliveries


type Applicability
    = Applicable
    | NotApplicable


type Closure
    = Open
    | PersonallyClosed String


type ProcessStatus
    = ActiveProcess
    | CompletedProcess


type Target
    = New
    | Existing Ref


type SaveState
    = Ready
    | Saving


type alias Ref =
    { id : String, version : Int }


type alias Placement =
    { x : Rating, y : Rating, dx : Float, dy : Float }


type alias OpportunityFields =
    { title : String
    , category : Category
    , kind : Kind
    , x : Rating
    , y : Rating
    , z : Rating
    , notes : String
    , source : String
    , proposed : Bool
    }


type CompletionProposal
    = NoCompletionProposal
    | CompletionProposed


type OpportunityStatus
    = ActiveOpportunity CompletionProposal
    | DiscardedOpportunity
    | CompletedOpportunity


type alias Opportunity =
    { id : String
    , fields : OpportunityFields
    , dx : Float
    , dy : Float
    , status : OpportunityStatus
    , version : Int
    , createdAt : String
    , updatedAt : String
    , review : Review
    }


type PendingStatus
    = AwaitingApproval User
    | Approved
    | CompletedBy User Review
    | LegacyCompleted


type PendingContent
    = PendingTask String PendingStatus
    | Separator String


type alias PendingFields =
    { content : PendingContent, owner : Owner }


type alias PendingShift =
    { revision : Int, steps : Int }


type PendingMove
    = KeepPosition
    | ShiftPosition PendingShift


type CompletionEdit
    = KeepCompletion
    | ChangeCompletion Completion


type PendingOrigin
    = Standalone
    | ProcessTask String String


type alias Pending =
    { id : String
    , fields : PendingFields
    , deleted : Bool
    , version : Int
    , createdAt : String
    , updatedAt : String
    , position : Int
    , origin : PendingOrigin
    }


type alias ProcessFields =
    { client : String, kind : ProcessKind, startDate : String, context : String }


type alias OwnerState =
    { owner : Owner
    , completion : Completion
    , review : Review
    }


type alias TaskState =
    { applicability : Applicability
    , completions : List OwnerState
    , evidence : Maybe String
    , notes : String
    , applicabilityReview : Review
    }


type alias Process =
    { id : String
    , fields : ProcessFields
    , status : ProcessStatus
    , states : Dict String TaskState
    , closure : Closure
    , legacySummary : String
    , templateVersion : Int
    , version : Int
    , createdAt : String
    , updatedAt : String
    }


type alias Task =
    { id : String
    , label : String
    , detail : String
    , owner : List Owner
    , allowNotApplicable : Bool
    , condition : String
    }


type alias Board =
    { opportunities : Dict String Opportunity
    , pending : Dict String Pending
    , processes : Dict String Process
    , nextId : Int
    , revision : Int
    , pendingOrderRevision : Int
    }



-- Historical tables remain on the backend and are included in full exports.


type alias Archive =
    { checks : String, snapshots : String }


type TaskChange
    = SetCompletion Owner Completion
    | ExcludeTask
    | RestoreTask
    | Acknowledge Checkbox


type Change
    = SaveOpportunity Target OpportunityFields
    | MoveOpportunity Ref Placement
    | ValidateOpportunity Ref
    | ConfirmOpportunity Ref
    | RemoveOpportunity Ref
    | RestoreOpportunity Ref
    | SuggestCompletion Ref CompletionProposal
    | CompleteOpportunity Ref
    | PurgeOpportunity Ref
    | SavePending Target PendingFields PendingMove CompletionEdit
    | MovePending Ref PendingShift
    | TogglePending Ref
    | RejectPendingCompletion Ref
    | ConfirmPending Ref
    | RemovePending Ref
    | RestorePending Ref
    | PurgePending Ref
    | PurgeCompleted Owner Ref
    | PurgeSeparator Ref
    | CreateProcess ProcessFields
    | SendToPending Ref String
    | WithdrawPending Ref String
    | ChangeTask Ref String TaskChange
    | CompleteProcess Ref
    | PurgeProcess Ref
    | SaveTaskNotes Ref String String


empty : Board
empty =
    { opportunities = Dict.empty
    , pending = Dict.empty
    , processes = Dict.empty
    , nextId = 1
    , revision = 0
    , pendingOrderRevision = 0
    }


emptyArchive : Archive
emptyArchive =
    { checks = "[]", snapshots = "[]" }


opportunityActive : Opportunity -> Bool
opportunityActive item =
    case item.status of
        ActiveOpportunity _ ->
            True

        _ ->
            False


opportunityDiscarded : Opportunity -> Bool
opportunityDiscarded item =
    item.status == DiscardedOpportunity


opportunityComplete : Opportunity -> Bool
opportunityComplete item =
    item.status == CompletedOpportunity


completionSuggested : Opportunity -> Bool
completionSuggested item =
    item.status == ActiveOpportunity CompletionProposed


pendingTitle : PendingContent -> String
pendingTitle content =
    case content of
        PendingTask title _ ->
            title

        Separator title ->
            title


pendingCompletion : PendingContent -> Maybe Completion
pendingCompletion content =
    case content of
        PendingTask _ status ->
            case status of
                CompletedBy _ _ ->
                    Just Done

                LegacyCompleted ->
                    Just Done

                _ ->
                    Just Todo

        Separator _ ->
            Nothing


pendingApproval : PendingContent -> Maybe User
pendingApproval content =
    case content of
        PendingTask _ (AwaitingApproval reviewer) ->
            Just reviewer

        _ ->
            Nothing


pendingReview : PendingContent -> Review
pendingReview content =
    case content of
        PendingTask _ (CompletedBy _ review) ->
            review

        _ ->
            Reviewed


pendingIsComplete : PendingContent -> Bool
pendingIsComplete content =
    case content of
        PendingTask _ (CompletedBy _ Reviewed) ->
            True

        PendingTask _ LegacyCompleted ->
            True

        _ ->
            False


orderedPending : Owner -> Board -> List Pending
orderedPending owner board =
    Dict.values board.pending
        |> List.filter (\item -> not item.deleted && not (pendingIsComplete item.fields.content) && item.fields.owner == owner)
        |> List.sortBy (\item -> ( item.position, item.createdAt, item.id ))


completedPending : Owner -> Board -> List Pending
completedPending owner board =
    Dict.values board.pending
        |> List.filter (\item -> not item.deleted && pendingIsComplete item.fields.content && item.fields.owner == owner)
        |> List.sortBy (\item -> ( item.updatedAt, item.id ))
        |> List.reverse


userName : User -> String
userName user =
    case user of
        UserOne ->
            "Rolo"

        UserTwo ->
            "Dot"


ref : { a | id : String, version : Int } -> Ref
ref item =
    { id = item.id, version = item.version }


rating : Rating -> Int
rating value =
    case value of
        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5


fromRating : Int -> Rating
fromRating value =
    if value <= 1 then
        One

    else if value == 2 then
        Two

    else if value == 3 then
        Three

    else if value == 4 then
        Four

    else
        Five


categoryLabel : Category -> String
categoryLabel value =
    case value of
        Ventex ->
            "Ventex"

        Contractor ->
            "Contratista"


kindLabel : Kind -> String
kindLabel value =
    case value of
        OpportunityKind ->
            "Oportunidad"

        Responsibility ->
            "Responsabilidad"


ownerLabel : Owner -> String
ownerLabel value =
    case value of
        Rolando ->
            "Rolo"

        Dot ->
            "Dot"

        Others ->
            "Otros"


processLabel : ProcessKind -> String
processLabel value =
    case value of
        NewOti ->
            "Nueva OTI"

        Deliveries ->
            "Entregas"



-- Keep template order, but never create two controls for the same responsible party.


taskOwners : Task -> List Owner
taskOwners task =
    List.filter (\owner -> List.member owner task.owner) [ Rolando, Dot, Others ]


initialTaskState : Task -> TaskState
initialTaskState task =
    { applicability = Applicable
    , completions = List.map (\owner -> OwnerState owner Todo Reviewed) (taskOwners task)
    , evidence = Nothing
    , notes = ""
    , applicabilityReview = Reviewed
    }


taskState : String -> Process -> TaskState
taskState id process =
    Dict.get id process.states
        |> Maybe.withDefault
            { applicability = Applicable
            , completions = []
            , evidence = Nothing
            , notes = ""
            , applicabilityReview = Reviewed
            }


ownerState : Owner -> TaskState -> OwnerState
ownerState owner state =
    state.completions
        |> List.filter (\part -> part.owner == owner)
        |> List.head
        |> Maybe.withDefault (OwnerState owner Todo Reviewed)


taskComplete : Task -> TaskState -> Bool
taskComplete task state =
    not (List.isEmpty (taskOwners task))
        && List.all (\owner -> (ownerState owner state).completion == Done) (taskOwners task)


linkedPending : String -> String -> Owner -> Board -> Maybe Pending
linkedPending processId taskId owner board =
    Dict.values board.pending
        |> List.filter (\item -> item.origin == ProcessTask processId taskId && item.fields.owner == owner)
        |> List.head


progress : Process -> { done : Int, applicable : Int, excluded : Int }
progress process =
    tasks process.fields.kind
        |> List.foldl
            (\task total ->
                let
                    state =
                        taskState task.id process
                in
                case state.applicability of
                    Applicable ->
                        { total
                            | done =
                                total.done
                                    + (if taskComplete task state then
                                        1

                                       else
                                        0
                                      )
                            , applicable = total.applicable + 1
                        }

                    NotApplicable ->
                        { total | excluded = total.excluded + 1 }
            )
            { done = 0, applicable = 0, excluded = 0 }


tasks : ProcessKind -> List Task
tasks kind =
    case kind of
        NewOti ->
            [ { id = "oti-01", label = "Comparar OC y cotización", detail = "Comparar OC y cotización: partidas, cantidades, precios, impuestos, pago y entrega.", owner = [ Dot ], allowNotApplicable = True, condition = "" }
            , { id = "oti-02", label = "Resolver diferencias", detail = "Resolver diferencias y corregir la cotización cuando corresponda.", owner = [ Rolando ], allowNotApplicable = True, condition = "Si hay diferencias entre la OC, cotización y condiciones acordadas." }
            , { id = "oti-03", label = "Crear OTI en Ventex", detail = "Crear la OTI en Ventex y proporcionar su PDF.", owner = [ Rolando ], allowNotApplicable = True, condition = "" }
            , { id = "oti-04", label = "Cotejar OTI", detail = "Cotejar la OTI contra los documentos aprobados.", owner = [ Dot ], allowNotApplicable = True, condition = "" }
            , { id = "oti-05", label = "Preparar acuse de OC", detail = "Preparar el acuse de OC.", owner = [ Dot ], allowNotApplicable = True, condition = "" }
            , { id = "oti-06", label = "Enviar acuse", detail = "Revisar y enviar el acuse.", owner = [ Rolando ], allowNotApplicable = True, condition = "" }
            , { id = "oti-07", label = "Confirmar equipo de Asana", detail = "Confirmar equipo y responsables de Asana.", owner = [ Rolando ], allowNotApplicable = True, condition = "" }
            , { id = "oti-08", label = "Crear proyecto de Asana", detail = "Crear el proyecto y sus tareas, adjuntar y verificar el PDF de OTI y marcarlo favorito.", owner = [ Dot ], allowNotApplicable = True, condition = "" }
            , { id = "oti-09", label = "Aportar documentos faltantes", detail = "Proporcionar los documentos que falten para el expediente.", owner = [ Rolando ], allowNotApplicable = True, condition = "Si faltan documentos para integrar el expediente." }
            , { id = "oti-10", label = "Organizar expediente", detail = "Organizar el expediente con OTI, cotización, OC y documentos relacionados; vincularlo a Asana.", owner = [ Dot ], allowNotApplicable = True, condition = "" }
            , { id = "oti-11", label = "Confirmar datos de venta", detail = "Confirmar los datos de venta que no aparezcan en los documentos, como descuentos ocultos.", owner = [ Rolando ], allowNotApplicable = True, condition = "Si existen datos de venta faltantes o ambiguos." }
            , { id = "oti-12", label = "Preparar JSON de ventas", detail = "Crear, validar y guardar el JSON de ventas.", owner = [ Dot ], allowNotApplicable = True, condition = "" }
            , { id = "oti-13", label = "Solicitar factura de anticipo", detail = "Iniciar la solicitud de factura de anticipo, si aplica.", owner = [ Dot ], allowNotApplicable = True, condition = "Si la venta requiere anticipo." }
            , { id = "oti-14", label = "Preparar solicitud REPSE", detail = "Preparar la solicitud REPSE a Lidia, si hay instalación.", owner = [ Dot ], allowNotApplicable = True, condition = "Si el alcance incluye instalación." }
            , { id = "oti-15", label = "Enviar solicitud REPSE", detail = "Revisar y enviar la solicitud REPSE.", owner = [ Rolando ], allowNotApplicable = True, condition = "Si el alcance incluye instalación." }
            , { id = "oti-16", label = "Proporcionar REPSE", detail = "Proporcionar los documentos REPSE. Responsable externo: Administración.", owner = [ Others ], allowNotApplicable = True, condition = "Si el alcance incluye instalación." }
            , { id = "oti-17", label = "Verificar y guardar REPSE", detail = "Verificar y guardar los documentos REPSE recibidos.", owner = [ Dot ], allowNotApplicable = True, condition = "Si el alcance incluye instalación." }
            ]

        Deliveries ->
            -- Keep persisted task IDs; the removed collection handoff used entrega-09.
            [ { id = "entrega-01", label = "Materiales Disponibles", detail = "Compras: entregar los materiales. Dot: precisar y documentar los materiales esperados. Rolando: verificar físicamente lo recibido.", owner = [ Dot, Rolando, Others ], allowNotApplicable = True, condition = "" }
            , { id = "entrega-02", label = "Integrar expediente", detail = "Dot: precisar y documentar la situación económica. Rolando: confirmar la información y solicitar la factura. Contabilidad: emitir y entregar la factura cuando sea necesaria.", owner = [ Rolando, Dot ], allowNotApplicable = True, condition = "" }
            , { id = "entrega-03", label = "Definir forma de envío", detail = "Dot: revisar y documentar las condiciones de envío. Rolando: acordar y confirmar la forma de envío.", owner = [ Rolando ], allowNotApplicable = True, condition = "" }
            , { id = "entrega-04", label = "Amarrar cuentas", detail = "Dot: verificar condiciones económicas para embarcar. Rolando: confirmar el pago o crédito para embarcar. Cliente: cubrir el pago acordado cuando corresponda.", owner = [ Dot, Rolando, Others ], allowNotApplicable = True, condition = "" }
            , { id = "entrega-05", label = "Aviso de envío", detail = "Dot: preparar el aviso de próximo envío. Rolando: revisar y enviar el aviso de próximo envío. Cliente: respuesta al correo.", owner = [ Dot, Rolando, Others ], allowNotApplicable = True, condition = "" }
            , { id = "entrega-06", label = "Documentos de envío", detail = "Dot: preparar la documentación del embarque. Rolando: revisar, imprimir y adjuntar documentación al embarque.", owner = [ Dot, Rolando ], allowNotApplicable = True, condition = "" }
            , { id = "entrega-07", label = "Instrucciones de embarque", detail = "Dot: preparar las instrucciones para el mensajero. Rolando: confirmar y comunicar instrucciones de envío. Chofer: envía y entrega comprobante.", owner = [ Dot, Rolando, Others ], allowNotApplicable = True, condition = "" }
            , { id = "entrega-08", label = "Confirmación de embarque", detail = "Dot: preparar la confirmación del embarque. Rolando: confirmar y enviar comunicado del embarque realizado.", owner = [ Rolando, Dot ], allowNotApplicable = True, condition = "" }
            , { id = "entrega-10", label = "JSON de cobranza para Comisiones", detail = "Dot: crear, validar y guardar el JSON de cobranza. Rolando: incorporar JSON al sistema de comisiones.", owner = [ Dot, Rolando ], allowNotApplicable = True, condition = "Si se recibió o confirmó un pago por motivo de la entrega." }
            ]

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
    | CompletionBox


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
    = Applicable Completion
    | NotApplicable Completion


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


type alias Pending =
    { id : String
    , fields : PendingFields
    , deleted : Bool
    , version : Int
    , createdAt : String
    , updatedAt : String
    , position : Int
    }


type alias ProcessFields =
    { client : String, kind : ProcessKind, startDate : String, context : String }


type alias TaskState =
    { applicability : Applicability
    , evidence : Maybe String
    , notes : String
    , applicabilityReview : Review
    , completionReview : Review
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
    , owner : String
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
    = SetCompletion Completion
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


taskState : String -> Process -> TaskState
taskState id process =
    Dict.get id process.states
        |> Maybe.withDefault
            { applicability = Applicable Todo
            , evidence = Nothing
            , notes = ""
            , applicabilityReview = Reviewed
            , completionReview = Reviewed
            }


progress : Process -> { done : Int, applicable : Int, excluded : Int }
progress process =
    tasks process.fields.kind
        |> List.foldl
            (\task total ->
                case (taskState task.id process).applicability of
                    Applicable Done ->
                        { total | done = total.done + 1, applicable = total.applicable + 1 }

                    Applicable Todo ->
                        { total | applicable = total.applicable + 1 }

                    NotApplicable _ ->
                        { total | excluded = total.excluded + 1 }
            )
            { done = 0, applicable = 0, excluded = 0 }


tasks : ProcessKind -> List Task
tasks kind =
    case kind of
        NewOti ->
            [ { id = "oti-01", label = "Comparar OC y cotización", detail = "Comparar OC y cotización: partidas, cantidades, precios, impuestos, pago y entrega.", owner = "dot", allowNotApplicable = True, condition = "" }
            , { id = "oti-02", label = "Resolver diferencias", detail = "Resolver diferencias y corregir la cotización cuando corresponda.", owner = "Rolando", allowNotApplicable = True, condition = "Si hay diferencias entre la OC, cotización y condiciones acordadas." }
            , { id = "oti-03", label = "Crear OTI en Ventex", detail = "Crear la OTI en Ventex y proporcionar su PDF.", owner = "Rolando", allowNotApplicable = True, condition = "" }
            , { id = "oti-04", label = "Cotejar OTI", detail = "Cotejar la OTI contra los documentos aprobados.", owner = "dot", allowNotApplicable = True, condition = "" }
            , { id = "oti-05", label = "Preparar acuse de OC", detail = "Preparar el acuse de OC.", owner = "dot", allowNotApplicable = True, condition = "" }
            , { id = "oti-06", label = "Enviar acuse", detail = "Revisar y enviar el acuse.", owner = "Rolando", allowNotApplicable = True, condition = "" }
            , { id = "oti-07", label = "Confirmar equipo de Asana", detail = "Confirmar equipo y responsables de Asana.", owner = "Rolando", allowNotApplicable = True, condition = "" }
            , { id = "oti-08", label = "Crear proyecto de Asana", detail = "Crear el proyecto y sus tareas, adjuntar y verificar el PDF de OTI y marcarlo favorito.", owner = "dot", allowNotApplicable = True, condition = "" }
            , { id = "oti-09", label = "Aportar documentos faltantes", detail = "Proporcionar los documentos que falten para el expediente.", owner = "Rolando", allowNotApplicable = True, condition = "Si faltan documentos para integrar el expediente." }
            , { id = "oti-10", label = "Organizar expediente", detail = "Organizar el expediente con OTI, cotización, OC y documentos relacionados; vincularlo a Asana.", owner = "dot", allowNotApplicable = True, condition = "" }
            , { id = "oti-11", label = "Confirmar datos de venta", detail = "Confirmar los datos de venta que no aparezcan en los documentos, como descuentos ocultos.", owner = "Rolando", allowNotApplicable = True, condition = "Si existen datos de venta faltantes o ambiguos." }
            , { id = "oti-12", label = "Preparar JSON de ventas", detail = "Crear, validar y guardar el JSON de ventas.", owner = "dot", allowNotApplicable = True, condition = "" }
            , { id = "oti-13", label = "Solicitar factura de anticipo", detail = "Iniciar la solicitud de factura de anticipo, si aplica.", owner = "dot", allowNotApplicable = True, condition = "Si la venta requiere anticipo." }
            , { id = "oti-14", label = "Preparar solicitud REPSE", detail = "Preparar la solicitud REPSE a Lidia, si hay instalación.", owner = "dot", allowNotApplicable = True, condition = "Si el alcance incluye instalación." }
            , { id = "oti-15", label = "Enviar solicitud REPSE", detail = "Revisar y enviar la solicitud REPSE.", owner = "Rolando", allowNotApplicable = True, condition = "Si el alcance incluye instalación." }
            , { id = "oti-16", label = "Proporcionar REPSE", detail = "Proporcionar los documentos REPSE.", owner = "Administración", allowNotApplicable = True, condition = "Si el alcance incluye instalación." }
            , { id = "oti-17", label = "Verificar y guardar REPSE", detail = "Verificar y guardar los documentos REPSE recibidos.", owner = "dot", allowNotApplicable = True, condition = "Si el alcance incluye instalación." }
            ]

        Deliveries ->
            [ { id = "entrega-01", label = "Entregar materiales", detail = "Entregar los materiales a Rolando.", owner = "Compras", allowNotApplicable = True, condition = "" }
            , { id = "entrega-02", label = "Preparar relación pendiente", detail = "Preparar la relación de referencias y cantidades pendientes.", owner = "dot", allowNotApplicable = True, condition = "" }
            , { id = "entrega-03", label = "Cotejar materiales", detail = "Cotejar físicamente los materiales y confirmar qué se enviará.", owner = "Rolando", allowNotApplicable = True, condition = "" }
            , { id = "entrega-04", label = "Revisar pago y factura", detail = "Revisar condiciones de pago y disponibilidad de factura.", owner = "dot", allowNotApplicable = True, condition = "" }
            , { id = "entrega-05", label = "Confirmar pago o crédito", detail = "Confirmar el pago recibido o la condición de crédito aplicable.", owner = "Rolando", allowNotApplicable = True, condition = "" }
            , { id = "entrega-06", label = "Solicitar factura faltante", detail = "Solicitar la factura faltante mediante la tarea de Asana correspondiente.", owner = "dot", allowNotApplicable = True, condition = "Si falta la factura." }
            , { id = "entrega-07", label = "Proporcionar factura", detail = "Emitir y proporcionar la factura.", owner = "Facturación", allowNotApplicable = True, condition = "Si falta la factura." }
            , { id = "entrega-08", label = "Preparar aviso previo", detail = "Verificar la factura y preparar el aviso de próximo envío, solicitando pago solo cuando corresponda.", owner = "dot", allowNotApplicable = True, condition = "" }
            , { id = "entrega-09", label = "Enviar aviso previo", detail = "Revisar y enviar el aviso.", owner = "Rolando", allowNotApplicable = True, condition = "" }
            , { id = "entrega-10", label = "Responder o dar visto bueno", detail = "Responder o dar visto bueno cuando se le haya solicitado.", owner = "Cliente", allowNotApplicable = True, condition = "Si se solicitó respuesta o visto bueno al cliente." }
            , { id = "entrega-11", label = "Preparar documentos", detail = "Preparar factura, OC cuando exista y remisión cuando el envío sea parcial; revisar los datos fiscales necesarios para el flete.", owner = "dot", allowNotApplicable = True, condition = "" }
            , { id = "entrega-12", label = "Imprimir documentos", detail = "Imprimir y reunir la documentación física.", owner = "Rolando", allowNotApplicable = True, condition = "" }
            , { id = "entrega-13", label = "Confirmar datos del envío", detail = "Confirmar destino, transporte, modalidad, cajas y contacto.", owner = "Rolando", allowNotApplicable = True, condition = "" }
            , { id = "entrega-14", label = "Preparar instrucciones", detail = "Preparar las instrucciones para el chofer.", owner = "dot", allowNotApplicable = True, condition = "" }
            , { id = "entrega-15", label = "Comunicar instrucciones", detail = "Comunicar las instrucciones al chofer.", owner = "Rolando", allowNotApplicable = True, condition = "" }
            , { id = "entrega-16", label = "Despachar y devolver guía", detail = "Entregar el embarque y devolver la guía.", owner = "Chofer", allowNotApplicable = True, condition = "" }
            , { id = "entrega-17", label = "Compartir y entregar guía", detail = "Fotografiar la guía, compartirla con dot y entregar el original a Lidia.", owner = "Rolando", allowNotApplicable = True, condition = "" }
            , { id = "entrega-18", label = "Preparar correo posterior", detail = "Preparar el correo posterior con guía, factura, OC y remisión cuando correspondan, con copia a Lidia.", owner = "dot", allowNotApplicable = True, condition = "" }
            , { id = "entrega-19", label = "Enviar correo posterior", detail = "Revisar y enviar ese correo.", owner = "Rolando", allowNotApplicable = True, condition = "" }
            , { id = "entrega-20", label = "Asignar cobranza", detail = "Crear o actualizar la tarea de cobranza asignada a Lidia, sin duplicarla.", owner = "dot", allowNotApplicable = True, condition = "" }
            , { id = "entrega-21", label = "Seguir saldo pendiente", detail = "Dar seguimiento al saldo pendiente según las condiciones acordadas.", owner = "Lidia", allowNotApplicable = True, condition = "Si existe saldo pendiente." }
            , { id = "entrega-22", label = "Confirmar datos del pago", detail = "Confirmar los datos de cualquier pago recibido.", owner = "Rolando", allowNotApplicable = True, condition = "Si hubo un pago real recibido." }
            , { id = "entrega-23", label = "Preparar JSON de cobranza", detail = "Crear, validar y guardar el JSON de cobranza cuando corresponda, evitando duplicados.", owner = "dot", allowNotApplicable = True, condition = "Si hubo cobranza o confirmación de un pago real." }
            ]

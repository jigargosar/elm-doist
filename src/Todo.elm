module Todo exposing
    ( CompareBy(..)
    , DueAt(..)
    , Filter(..)
    , Msg(..)
    , Todo
    , TodoDict
    , TodoList
    , compareDueDate
    , concatCompareBy
    , decoder
    , dueAtDecoder
    , dueAtEncoder
    , dueAtFromPosix
    , dueAtToMillis
    , dueDateEq
    , encoder
    , filter
    , filterSort
    , formatDueAt
    , listDecoder
    , listEncoder
    , matchesFilter
    , new
    , notDue
    , patchTodo
    , sortWith
    )

import Calendar
import Compare exposing (Comparator)
import Date
import Dict exposing (Dict)
import Fire
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import Maybe.Extra as MX
import Millis exposing (Millis)
import ProjectId exposing (ProjectId)
import Time
import TodoId exposing (TodoId)


type DueAt
    = DueAt Millis
    | NoDue


dueAtFromPosix : Time.Posix -> DueAt
dueAtFromPosix now =
    DueAt (Time.posixToMillis now)


notDue : DueAt
notDue =
    NoDue


dueAtDecoder : Decoder DueAt
dueAtDecoder =
    let
        tagDecoder tag =
            case tag of
                "DueAt" ->
                    JD.field "millis" JD.int
                        |> JD.map DueAt

                "NoDue" ->
                    JD.succeed NoDue

                _ ->
                    JD.fail ("Invalid dueAt tag: " ++ tag)
    in
    JD.oneOf
        [ JD.int |> JD.map DueAt
        , JD.null NoDue
        , JD.field "tag" JD.string |> JD.andThen tagDecoder
        ]


dueAtEncoder : DueAt -> Value
dueAtEncoder due =
    case due of
        DueAt millis ->
            JE.object
                [ ( "tag", JE.string "DueAt" )
                , ( "millis", JE.int millis )
                ]

        NoDue ->
            JE.object
                [ ( "tag", JE.string "NoDue" )
                ]


type alias Todo =
    { id : TodoId
    , title : String
    , sortIdx : Int
    , projectId : ProjectId
    , projectIdModifiedAt : Millis
    , isDone : Bool
    , dueAt : DueAt
    , createdAt : Millis
    , modifiedAt : Millis
    }


decoder : Decoder Todo
decoder =
    JD.succeed Todo
        |> JDP.required "id" TodoId.decoder
        |> JDP.required "title" JD.string
        |> JDP.required "sortIdx" JD.int
        |> JDP.optional "projectId" ProjectId.decoder ProjectId.default
        |> JDP.optional "projectIdModifiedAt" JD.int 0
        |> JDP.required "isDone" JD.bool
        |> JDP.optional "dueAt" dueAtDecoder NoDue
        |> JDP.required "createdAt" JD.int
        |> JDP.required "modifiedAt" JD.int


encoder : Todo -> Value
encoder { id, title, sortIdx, projectId, projectIdModifiedAt, isDone, dueAt, createdAt, modifiedAt } =
    JE.object
        [ ( "id", TodoId.encoder id )
        , ( "title", JE.string title )
        , ( "sortIdx", JE.int sortIdx )
        , ( "projectId", ProjectId.encoder projectId )
        , ( "projectIdModifiedAt", JE.int projectIdModifiedAt )
        , ( "isDone", JE.bool isDone )
        , ( "dueAt", dueAtEncoder dueAt )
        , ( "createdAt", JE.int createdAt )
        , ( "modifiedAt", JE.int modifiedAt )
        ]


type Msg
    = SetCompleted Bool
    | SetProjectId ProjectId
    | SetTitle String
    | SetSortIdx Int
    | SetDueAt DueAt


new : Time.Posix -> String -> DueAt -> ProjectId -> Value
new now title dueAt projectId =
    let
        nowMillis =
            Time.posixToMillis now
    in
    { id = TodoId.new
    , title = title
    , sortIdx = 0
    , projectId = projectId
    , projectIdModifiedAt = nowMillis
    , isDone = False
    , dueAt = dueAt
    , createdAt = nowMillis
    , modifiedAt = nowMillis
    }
        |> encoder


patchTodo : Time.Posix -> TodoId -> List Msg -> Cmd msg
patchTodo now todoId todoMsgList =
    createPatches now todoMsgList
        |> MX.unwrap Cmd.none (Fire.updateTodo todoId)


createPatches : Time.Posix -> List Msg -> Maybe (List ( String, Value ))
createPatches now msgList =
    if List.isEmpty msgList then
        Nothing

    else
        let
            nowMillis =
                Time.posixToMillis now
        in
        (( "modifiedAt", JE.int nowMillis )
            :: List.concatMap (createPatch nowMillis) msgList
        )
            |> Just


createPatch : Int -> Msg -> List ( String, Value )
createPatch now msg =
    case msg of
        SetCompleted bool ->
            [ ( "isDone", JE.bool bool ) ]

        SetProjectId projectId ->
            [ ( "projectId", ProjectId.encoder projectId )
            , ( "projectIdModifiedAt", JE.int now )
            ]

        SetTitle title ->
            [ ( "title", JE.string title ) ]

        SetSortIdx sortIdx ->
            [ ( "sortIdx", JE.int sortIdx ) ]

        SetDueAt dueAt ->
            [ ( "dueAt", dueAt |> dueAtEncoder ) ]


dueAtToMillis : DueAt -> Maybe Millis
dueAtToMillis dueAt =
    case dueAt of
        NoDue ->
            Nothing

        DueAt mi ->
            mi |> Just


dueAtToPosix : DueAt -> Maybe Time.Posix
dueAtToPosix dueAt =
    case dueAt of
        NoDue ->
            Nothing

        DueAt mi ->
            Just (Time.millisToPosix mi)


formatDueAt : String -> Time.Zone -> DueAt -> Maybe String
formatDueAt formatStr zone =
    dueAtToPosix >> Maybe.map (Date.fromPosix zone >> Date.format formatStr)


dueDateEq : Calendar.Date -> Todo -> Bool
dueDateEq dt model =
    case model.dueAt of
        NoDue ->
            False

        DueAt mi ->
            mi
                |> Time.millisToPosix
                |> Calendar.fromPosix
                |> (==) dt


compareDueDate : Calendar.Date -> Todo -> Maybe Order
compareDueDate dt model =
    case model.dueAt of
        NoDue ->
            Nothing

        DueAt mi ->
            mi
                |> Time.millisToPosix
                |> Calendar.fromPosix
                |> Calendar.compare dt
                |> Just


type Filter
    = Pending
    | Completed
    | BelongsToProject ProjectId
    | NotInProject ProjectId
    | AndFilter Filter Filter


matchesFilter : Filter -> Todo -> Bool
matchesFilter filter_ todo =
    case filter_ of
        Pending ->
            not todo.isDone

        Completed ->
            todo.isDone

        BelongsToProject pid ->
            todo.projectId == pid

        NotInProject pid ->
            todo.projectId /= pid

        AndFilter a b ->
            matchesFilter a todo && matchesFilter b todo


type alias TodoList =
    List Todo


type alias TodoDict =
    Dict TodoId Todo


listDecoder : Decoder TodoList
listDecoder =
    JD.list decoder


listEncoder : TodoList -> Value
listEncoder =
    JE.list encoder


filter : Filter -> TodoList -> TodoList
filter filter_ =
    List.filter (matchesFilter filter_)


type CompareBy
    = ByIdx
    | ByRecentlyModified
    | ByRecentlyCreated
    | ByRecentlyModifiedProjectId


toComparator : CompareBy -> Comparator Todo
toComparator compareBy =
    case compareBy of
        ByIdx ->
            Compare.by .sortIdx

        ByRecentlyModified ->
            Compare.by .modifiedAt |> Compare.reverse

        ByRecentlyCreated ->
            Compare.by .createdAt |> Compare.reverse

        ByRecentlyModifiedProjectId ->
            Compare.by .projectIdModifiedAt |> Compare.reverse


concatCompareBy : List CompareBy -> Comparator Todo
concatCompareBy comps =
    Compare.concat (List.map toComparator comps)


sortWith : List CompareBy -> TodoList -> TodoList
sortWith comps =
    List.sortWith (concatCompareBy comps)


filterSort : Filter -> List CompareBy -> TodoList -> TodoList
filterSort fil comps =
    filter fil >> sortWith comps

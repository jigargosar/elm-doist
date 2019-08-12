module Todo exposing
    ( CompareBy(..)
    , DueAt
    , Filter(..)
    , Msg(..)
    , Todo
    , TodoDict
    , TodoList
    , concatCompareBy
    , decoder
    , dueAtDecoder
    , dueAtEncoder
    , encoder
    , filter
    , filterSingle
    , filterSort
    , listDecoder
    , listEncoder
    , matchesFilter
    , modify
    , modifyPatch
    , newForProject
    , newToday
    , sortWith
    )

import Compare exposing (Comparator)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import Maybe.Extra
import Millis exposing (Millis)
import ProjectId exposing (ProjectId)
import TodoId exposing (TodoId)


type alias DueAt =
    Maybe Millis


dueAtDecoder : Decoder DueAt
dueAtDecoder =
    JD.nullable JD.int


dueAtEncoder : DueAt -> Value
dueAtEncoder =
    Maybe.Extra.unwrap JE.null JE.int


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
        |> JDP.required "id" JD.string
        |> JDP.required "title" JD.string
        |> JDP.required "sortIdx" JD.int
        |> JDP.optional "projectId" ProjectId.decoder ProjectId.default
        |> JDP.optional "projectIdModifiedAt" JD.int 0
        |> JDP.required "isDone" JD.bool
        |> JDP.optional "dueAt" dueAtDecoder Nothing
        |> JDP.required "createdAt" JD.int
        |> JDP.required "modifiedAt" JD.int


encoder : Todo -> Value
encoder { id, title, sortIdx, projectId, projectIdModifiedAt, isDone, dueAt, createdAt, modifiedAt } =
    JE.object
        [ ( "id", JE.string id )
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


newForProject : Millis -> ProjectId -> Value
newForProject now pid =
    { id = ""
    , title = ""
    , sortIdx = 0
    , projectId = pid
    , projectIdModifiedAt = now
    , isDone = False
    , dueAt = Nothing
    , createdAt = now
    , modifiedAt = now
    }
        |> encoder


newToday : Millis -> Millis -> Value
newToday now dueAt =
    { id = ""
    , title = ""
    , sortIdx = 0
    , projectId = ProjectId.default
    , projectIdModifiedAt = now
    , isDone = False
    , dueAt = Just dueAt
    , createdAt = now
    , modifiedAt = now
    }
        |> encoder


modifyPatch : Msg -> Millis -> List ( String, Value )
modifyPatch msg now =
    ( "modifiedAt", JE.int now )
        :: (case msg of
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
           )


update : Msg -> Todo -> Todo
update msg model =
    case msg of
        SetCompleted bool ->
            { model | isDone = bool }

        SetProjectId projectId ->
            { model | projectId = projectId }

        SetTitle title ->
            { model | title = title }

        SetSortIdx sortIdx ->
            { model | sortIdx = sortIdx }

        SetDueAt dueAt ->
            { model | dueAt = dueAt }


modifyWithNow : Millis -> Msg -> Todo -> Maybe Todo
modifyWithNow now msg model =
    let
        newModel =
            update msg model
    in
    if newModel == model then
        Nothing

    else
        newModel |> setModifiedAt now |> Just


modify : Msg -> Millis -> Todo -> Maybe Todo
modify msg now model =
    modifyWithNow now msg model


setModifiedAt now todo =
    { todo | modifiedAt = now }


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


filterSingle : Filter -> Todo -> Maybe Todo
filterSingle filter_ =
    List.singleton >> filter filter_ >> List.head


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

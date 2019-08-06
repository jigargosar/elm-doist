module Todo exposing
    ( CompareBy(..)
    , Filter(..)
    , Msg(..)
    , Todo
    , TodoDict
    , TodoList
    , decoder
    , encoder
    , filter
    , filterSingle
    , filterSort
    , listDecoder
    , listEncoder
    , matchesFilter
    , modify
    , relaxedDecoder
    )

import Compare exposing (Comparator)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode as JE exposing (Value)
import ProjectId exposing (ProjectId)
import TodoId exposing (TodoId)


type alias Millis =
    Int


type alias Todo =
    { id : TodoId
    , title : String
    , sortIdx : Int
    , projectId : ProjectId
    , isDone : Bool
    , createdAt : Int
    , modifiedAt : Int
    }


decoder : Decoder Todo
decoder =
    JD.succeed Todo
        |> JDP.required "id" JD.string
        |> JDP.required "title" JD.string
        |> JDP.required "sortIdx" JD.int
        |> JDP.optional "projectId" ProjectId.decoder ProjectId.default
        |> JDP.required "isDone" JD.bool
        |> JDP.required "createdAt" JD.int
        |> JDP.required "modifiedAt" JD.int


relaxedDecoder : Decoder Todo
relaxedDecoder =
    JD.succeed Todo
        |> JDP.required "id" JD.string
        |> JDP.custom (JD.oneOf [ JD.field "title" JD.string, JD.field "name" JD.string ])
        |> JDP.optional "sortIdx" JD.int 0
        |> JDP.optional "projectId" ProjectId.decoder ProjectId.default
        |> JDP.optional "isDone" JD.bool False
        |> JDP.required "createdAt" JD.int
        |> JDP.custom (JD.oneOf [ JD.field "modifiedAt" JD.int, JD.field "createdAt" JD.int ])


encoder : Todo -> Value
encoder { id, title, sortIdx, projectId, isDone, createdAt, modifiedAt } =
    JE.object
        [ ( "id", JE.string id )
        , ( "title", JE.string title )
        , ( "sortIdx", JE.int sortIdx )
        , ( "projectId", ProjectId.encoder projectId )
        , ( "isDone", JE.bool isDone )
        , ( "createdAt", JE.int createdAt )
        , ( "modifiedAt", JE.int modifiedAt )
        ]


type Msg
    = SetCompleted Bool
    | SetProjectId ProjectId
    | SetTitle String
    | SetSortIdx Int


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


toComparator : CompareBy -> Comparator Todo
toComparator compareBy =
    case compareBy of
        ByIdx ->
            Compare.by .sortIdx

        ByRecentlyModified ->
            Compare.by .modifiedAt |> Compare.reverse


sortWith : List CompareBy -> TodoList -> TodoList
sortWith comps =
    List.sortWith (Compare.concat (List.map toComparator comps))


filterSort : Filter -> List CompareBy -> TodoList -> TodoList
filterSort fil comps =
    filter fil >> sortWith comps

module SelectProject exposing (Model, Msg, init, update, view)

import BasicsExtra exposing (eq_)
import Css
import Focus
import Html.Styled as H exposing (Attribute, Html, div, text)
import Html.Styled.Attributes exposing (class, css, tabindex)
import Html.Styled.Events as E
import HtmlExtra as HX
import Json.Decode as JD
import List.Extra as LX
import Maybe.Extra as MX
import Project exposing (Project, ProjectList)
import ProjectId exposing (ProjectId)
import Task
import UI.Key as Key
import UI.TextButton as TextButton


type Model
    = DropDownOpen Bool


type alias Internal =
    {}


init : Model
init =
    DropDownOpen False


focusFirstCmd : Cmd Msg
focusFirstCmd =
    Focus.attempt Focused firstDomId


type Msg
    = OpenMenu
    | Selected ProjectId
    | CloseMenu
    | Focused Focus.FocusResult


update : { toMsg : Msg -> msg, onSelect : ProjectId -> msg } -> Msg -> Model -> ( Model, Cmd msg )
update config message model =
    case message of
        OpenMenu ->
            ( DropDownOpen True, focusFirstCmd |> Cmd.map config.toMsg )

        CloseMenu ->
            ( DropDownOpen False, Cmd.none )

        Selected projectId ->
            ( DropDownOpen False, config.onSelect projectId |> perform )

        Focused _ ->
            ( model, Cmd.none )


perform : a -> Cmd a
perform =
    Task.succeed >> Task.perform identity


type alias DisplayProject =
    { id : ProjectId
    , title : String
    }


toDisplayProject : Project -> DisplayProject
toDisplayProject { id, title } =
    { id = id, title = title }


inboxDisplayProject : DisplayProject
inboxDisplayProject =
    { id = ProjectId.default, title = "Inbox" }


getDisplayProjectTitle : ProjectId -> List DisplayProject -> String
getDisplayProjectTitle projectId projectList =
    LX.find (.id >> eq_ projectId) projectList
        |> MX.unwrap "<Unknown Project>" .title


findSplit : (a -> Bool) -> List a -> Maybe ( List a, a, List a )
findSplit pred list =
    let
        ( l, c, r ) =
            findSplitHelp pred list
    in
    Maybe.map (\jc -> ( l, jc, r )) c


findSplitHelp : (a -> Bool) -> List a -> ( List a, Maybe a, List a )
findSplitHelp pred =
    List.foldl
        (\item ( l, c, r ) ->
            case c of
                Just _ ->
                    ( l, c, item :: r )

                Nothing ->
                    if pred item then
                        ( l, Just item, r )

                    else
                        ( item :: l, c, r )
        )
        ( [], Nothing, [] )


view : ProjectId -> ProjectList -> Model -> H.Html Msg
view selectedProjectId projectList model =
    let
        displayList =
            inboxDisplayProject
                :: List.map toDisplayProject projectList

        open =
            case model of
                DropDownOpen bool ->
                    bool

        pivot =
            findSplit (.id >> eq_ selectedProjectId) displayList
                |> MX.unpack (\_ -> ( [], inboxDisplayProject, List.drop 1 displayList ))
                    identity
    in
    viewSelectInput
        { itemLabel = .title
        , view =
            \{ id, title } attrs ->
                viewMenuItem attrs (Selected id) title
        , onClose = CloseMenu
        , onOpen = OpenMenu
        , onSelect = \{ id } -> Selected id
        }
        { open = open, items = pivot }


viewSelectInput :
    { itemLabel : item -> String
    , view : item -> List (Attribute msg) -> Html msg
    , onClose : msg
    , onOpen : msg
    , onSelect : item -> msg
    }
    -> { open : Bool, items : ( List item, item, List item ) }
    -> Html msg
viewSelectInput config props =
    let
        ( left, selected, right ) =
            props.items

        allItems =
            left ++ [ selected ] ++ right

        firstItem =
            List.head left |> Maybe.withDefault selected

        selectedItemStyle item =
            if item == selected then
                Css.batch [ Css.fontWeight Css.bold ]

            else
                Css.batch []

        attrsForItem item =
            [ HX.idIf (item == firstItem) (always firstDomId), css [ selectedItemStyle item ] ]

        viewItem_ item =
            config.view item (attrsForItem item)
    in
    div [ class "relative" ]
        [ div [ E.onClick config.onOpen ]
            [ text "project: "
            , text (config.itemLabel selected)
            ]
        , if props.open then
            H.styled (H.node "track-focus-outside")
                []
                [ class "absolute top-1 shadow-1 bg-white"
                , E.on "focusOutside" (JD.succeed config.onClose)
                , Key.onEscape config.onClose
                , tabindex -1
                ]
                (List.map viewItem_ allItems)

          else
            text ""
        ]


viewMenuItem : List (Attribute msg) -> msg -> String -> Html msg
viewMenuItem attrs =
    TextButton.view (class "pa2" :: attrs)


firstDomId =
    "select-project__first-dom-id"

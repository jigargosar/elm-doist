module SelectProject exposing (Model, Msg, init, update, view)

import BasicsExtra exposing (eq_)
import Css
import Focus
import Html.Styled as H exposing (Attribute, Html, div, text)
import Html.Styled.Attributes as A exposing (class, css, tabindex)
import Html.Styled.Events as E
import HtmlExtra as HX
import Json.Decode as JD
import Maybe.Extra as MX
import Project exposing (Project, ProjectList)
import ProjectId exposing (ProjectId)
import Task
import UI.Key as Key
import UI.TextButton as TextButton


type Model
    = IsOpen Bool


type alias Internal =
    {}


init : Model
init =
    IsOpen False


focusFirstCmd : Cmd Msg
focusFirstCmd =
    Focus.attempt Focused firstDomId


type Msg
    = OpenPopup
    | Selected ProjectId
    | ClosePopup
    | Focused Focus.FocusResult


update : { toMsg : Msg -> msg, onSelect : ProjectId -> msg } -> Msg -> Model -> ( Model, Cmd msg )
update config message model =
    case message of
        OpenPopup ->
            ( IsOpen True, focusFirstCmd |> Cmd.map config.toMsg )

        ClosePopup ->
            ( IsOpen False, Cmd.none )

        Selected projectId ->
            ( IsOpen False, config.onSelect projectId |> perform )

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


view : ProjectId -> ProjectList -> Model -> H.Html Msg
view selectedProjectId projectList model =
    let
        displayList =
            inboxDisplayProject
                :: List.map toDisplayProject projectList

        open =
            case model of
                IsOpen bool ->
                    bool

        pivot =
            zipperFromListFocusedBy (.id >> eq_ selectedProjectId) displayList
                |> MX.unpack (\_ -> zipperFromCons inboxDisplayProject (List.drop 1 displayList))
                    identity
    in
    viewSelectInput
        { attrs = [ A.id "select-project" ]
        , itemLabel = .title
        , onClose = ClosePopup
        , onOpen = OpenPopup
        , onSelect = \{ id } -> Selected id
        }
        { open = open, items = pivot }


viewSelectInput :
    { attrs : List (Attribute msg)
    , itemLabel : item -> String
    , onClose : msg
    , onOpen : msg
    , onSelect : item -> msg
    }
    -> { open : Bool, items : ( List item, item, List item ) }
    -> Html msg
viewSelectInput config props =
    let
        selectedItem =
            zipperFocus props.items

        allItems =
            zipperToList props.items

        firstItem =
            List.head allItems |> Maybe.withDefault selectedItem

        selectedItemStyle item =
            if item == selectedItem then
                Css.batch [ Css.fontWeight Css.bold ]

            else
                Css.batch []

        attrsForItem item =
            [ HX.idIf (item == firstItem) (always firstDomId)
            , css [ selectedItemStyle item ]
            ]

        viewItem item =
            viewMenuItem (attrsForItem item) (config.onSelect item) (config.itemLabel item)
    in
    div [ class "relative" ]
        [ div [ E.onClick config.onOpen ]
            [ text (config.itemLabel selectedItem)
            ]
        , if props.open then
            H.styled (H.node "track-focus-outside")
                []
                [ class "absolute top-1 left--1 shadow-1 bg-white"
                , E.on "focusOutside" (JD.succeed config.onClose)
                , Key.onEscape config.onClose
                , tabindex -1
                ]
                (List.map viewItem allItems)

          else
            text ""
        ]


viewMenuItem : List (Attribute msg) -> msg -> String -> Html msg
viewMenuItem attrs =
    TextButton.view (class "pa2" :: attrs)


firstDomId =
    "select-project__first-dom-id"



-- ZIPPER


zipperFromListFocusedBy : (a -> Bool) -> List a -> Maybe ( List a, a, List a )
zipperFromListFocusedBy pred list =
    let
        ( l, c, r ) =
            zipperFromListByHelp pred list
    in
    Maybe.map (\jc -> ( l, jc, List.reverse r )) c


zipperToList : ( List a, a, List a ) -> List a
zipperToList ( l, c, r ) =
    List.reverse l ++ [ c ] ++ r


zipperFocus : ( List a, a, List a ) -> a
zipperFocus ( _, c, _ ) =
    c


zipperFromCons : a -> List a -> ( List a, a, List a )
zipperFromCons first list =
    ( [], first, list )


zipperFromListByHelp : (a -> Bool) -> List a -> ( List a, Maybe a, List a )
zipperFromListByHelp pred =
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

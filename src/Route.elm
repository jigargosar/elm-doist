module Route exposing (Route(..), fromUrl, inboxUrl, projectUrl, todayUrl)

import ProjectId exposing (ProjectId)
import Url exposing (Url)
import Url.Builder as B
import Url.Parser exposing ((</>), Parser, custom, int, map, oneOf, parse, s, string, top)


type Route
    = Inbox
    | Project ProjectId
    | Today
    | NotFound Url


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Inbox top
        , map Inbox (s "inbox")
        , map Today (s "today")
        , map Project (s "project" </> projectIdParser)
        ]


projectIdParser =
    custom "ProjectId" <| ProjectId.fromString


fromUrl : Url -> Route
fromUrl url =
    parse routeParser url
        |> Maybe.withDefault (NotFound url)


projectUrl : ProjectId -> String
projectUrl pid =
    B.absolute [ "project", ProjectId.toString pid ] []


inboxUrl : String
inboxUrl =
    B.absolute [ "inbox" ] []


todayUrl : String
todayUrl =
    B.absolute [ "today" ] []

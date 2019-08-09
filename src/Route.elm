module Route exposing (Route(..), fromUrl, inboxUrl, projectUrl, todayUrl)

import Url exposing (Url)
import Url.Builder as B
import Url.Parser exposing ((</>), Parser, int, map, oneOf, parse, s, string, top)


type Route
    = Inbox
    | Project String
    | Today
    | NotFound Url


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Inbox top
        , map Inbox (s "inbox")
        , map Today (s "today")
        , map Project (s "project" </> string)
        ]


fromUrl : Url -> Route
fromUrl url =
    parse routeParser url
        |> Maybe.withDefault (NotFound url)


projectUrl : String -> String
projectUrl pid =
    B.absolute [ "project", pid ] []


inboxUrl : String
inboxUrl =
    B.absolute [ "inbox" ] []


todayUrl : String
todayUrl =
    B.absolute [ "today" ] []

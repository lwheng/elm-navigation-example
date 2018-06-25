-- A simple navigation example
-- Make sure you open up your browser's Console to see the logs I'm printing with `Debug.log`


module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import Navigation as Navigation
import String as String
import Task as Task
import UrlParser as UrlParser exposing ((</>))


main : Program Decode.Value Model Msg
main =
    Navigation.programWithFlags urlChange
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }



-- There is actually no need to write this as a separate function
-- I only did it so that the types are obvious


urlChange : Navigation.Location -> Msg
urlChange =
    Debug.log "UrlChange triggered at main" >> UrlChange



-- A Union type that defines the URLs possible for your site
-- Why define a Union type? When you operate using types (instead of String),
-- the compiler will complain to you if you left one case out (in a `case` block)


type Route
    = Home
    | Login
    | Book Int



-- A parser for our site's URLs
-- See http://package.elm-lang.org/packages/evancz/url-parser/latest/ for more details


parser : UrlParser.Parser (Route -> a) a
parser =
    UrlParser.oneOf
        [ UrlParser.map Home UrlParser.top
        , UrlParser.map Login (UrlParser.s "login")
        , UrlParser.map Book (UrlParser.s "book" </> UrlParser.int)
        ]



-- A helper function to parse Location to Route
-- Notice that I am using parseHash instead of parsePath
-- That is because in this example, my URLs look something like localhost:8000/src/Main.elm#/login
-- NOTE: I'm using elm-reactor. Normally, the URL should be like localhost:8000/#/login


fromLocation : Navigation.Location -> Maybe Route
fromLocation =
    UrlParser.parseHash parser


routeToString : Route -> String
routeToString r =
    let
        tokens =
            case r of
                Home ->
                    []

                Login ->
                    [ "login" ]

                Book bookID ->
                    [ "book", toString bookID ]
    in
        "#/" ++ (String.join "/" tokens)



-- Our Model. We keep track of the current Route and a simple message


type alias Model =
    { current : Route
    , message : String
    }


model : Model
model =
    { current = Home
    , message = ""
    }



-- Definition of Msg
-- UrlChange to deal with changes
-- MyTask may be a custom msg that you might have that calls a HTTP GET or HTTP POST
-- Then you probably have a `MyTaskResponse (Result String YourType)`


type Msg
    = UrlChange Navigation.Location
    | MyTask Route


type Token
    = AuthToken String


decodeToken : Decode.Value -> Maybe Token
decodeToken val =
    -- Imagine decoding an auth token saved in storage
    Just <| AuthToken "example"



-- Recall that in `main` we specified `urlChange` for `programWithFlags`
-- In `init`, if you do not perform any URL changes by `Navigation.newUrl` or `Navigation.modifyUrl` etc, `urlChange` is not triggered
-- i.e. if you enter an URL in the address bar and hit Enter, it goes into `init` and does not trigger `urlChange`
-- You do have the Location entered in the address bar, and you may choose to perform some actions based on its value
--
-- In this example of mine, if the user visits `#/login` and if the auth token is available, I forward the user to `#/book/20` by using `modifyUrl`
-- That, in turn, will trigger `urlChange` (i.e. UrlChange)


init : Decode.Value -> Navigation.Location -> ( Model, Cmd Msg )
init val location =
    case decodeToken val of
        Just _ ->
            case fromLocation <| Debug.log "init got location: " location of
                Just Login ->
                    { model | message = "Rewrite URL to Book 20" } ! [ routeToString (Book 20) |> Navigation.modifyUrl ]

                _ ->
                    model ! []

        Nothing ->
            model ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MyTask r ->
            { model | message = toString r } ! []

        UrlChange loc ->
            case fromLocation <| Debug.log "UrlChange handled in update" loc of
                Nothing ->
                    { model | message = "Unrecognized hash path" } ! []

                Just r ->
                    { model | current = r } ! [ Task.succeed (MyTask r) |> Task.perform identity ]


view : Model -> Html Msg
view model =
    div []
        [ text <| "Current = " ++ toString model.current
        , p [] []
        , a [ href <| routeToString Home ] [ text <| toString Home ]
        , p [] []
        , a [ href <| routeToString Login ] [ text <| toString Login ]
        , p [] []
        , a [ href <| routeToString <| Book 10 ] [ text <| toString <| Book 10 ]
        , p [] []
        , text <| "Message = " ++ model.message
        ]

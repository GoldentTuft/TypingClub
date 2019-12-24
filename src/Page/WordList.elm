module Page.WordList exposing (Model, Msg, init, update, view)

import API
import Env exposing (Env)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>), Parser, map, oneOf, s, top)
import Url.Parser.Query as Q



-- PROGRAM
-- MODEL


type alias Model =
    { state : State
    }


type State
    = Init
    | Loaded API.WordList
    | Error Http.Error


init : Env -> ( Model, Cmd Msg )
init env =
    ( Model Init
    , API.getAllWordList ReceiveAllWordList
    )



-- UPDATE


type Msg
    = ReceiveAllWordList (Result Http.Error API.WordList)


update : Msg -> Model -> Env -> ( Model, Cmd Msg, Env )
update msg model env =
    case msg of
        ReceiveAllWordList (Ok wl) ->
            ( { model | state = Loaded wl }, Cmd.none, env )

        ReceiveAllWordList (Err e) ->
            ( { model | state = Error e }, Cmd.none, env )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "element-panel" ]
        [ case model.state of
            Init ->
                div []
                    [ text "Loading WordList..." ]

            Loaded wl ->
                viewWordList wl

            Error e ->
                text ""
        ]


viewWordSummary : API.WordSummary -> Html Msg
viewWordSummary ws =
    li [ class "word-summary" ]
        [ a [ class " word-summary__word", href ("./?id=" ++ String.fromInt ws.id ++ "#typeLongWord") ] [ text ws.title ]
        , div [ class "word-summary__by" ] [ text ("by\u{00A0}" ++ ws.userName) ]
        ]


viewWordList : API.WordList -> Html Msg
viewWordList wl =
    ul [ class "word-list" ] (List.map viewWordSummary wl)

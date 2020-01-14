module Page.Etc exposing (Model, Msg, init, subscriptions, update, view)

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


init : Env -> ( Model, Cmd Msg )
init env =
    ( Model Init
    , Cmd.none
    )



-- UPDATE


type Msg
    = None


update : Msg -> Model -> Env -> ( Model, Cmd Msg, Env )
update msg model env =
    case msg of
        None ->
            ( { model | state = Init }, Cmd.none, env )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "element-panel" ]
        [ viewLink model
        ]


viewLink : Model -> Html Msg
viewLink model =
    div []
        [ a [ href "./#typeAny" ] [ text "何でもいいタイピング" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Sub.none

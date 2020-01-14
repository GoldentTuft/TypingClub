module Page.TypeAny exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Events exposing (onKeyDown)
import Env exposing (Env)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Round
import Task
import Time
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>), Parser, map, oneOf, s, top)
import Url.Parser.Query as Q



-- MODEL


type alias Model =
    { input : String
    , bestKPM : Float
    , nowKPM : Float
    , lastTime : Time.Posix
    }


init : Env -> ( Model, Cmd Msg )
init env =
    ( Model "" 0 0 (Time.millisToPosix 0)
    , Cmd.none
    )


reset : Model -> Model
reset model =
    Model "" 0 0 (Time.millisToPosix 0)



-- UPDATE


run : msg -> Cmd msg
run m =
    Task.perform (always m) (Task.succeed ())


type Msg
    = KeyDown String
    | TenSecond Time.Posix
    | OneSecond Time.Posix


update : Msg -> Model -> Env -> ( Model, Cmd Msg, Env )
update msg model env =
    case msg of
        KeyDown key ->
            case key of
                "Escape" ->
                    ( reset model, Cmd.none, env )

                _ ->
                    ( { model | input = model.input ++ String.left 1 key }, Cmd.none, env )

        OneSecond time ->
            let
                newModel =
                    updateKPM time model
            in
            ( { newModel | input = "" }, Cmd.none, env )

        TenSecond time ->
            ( model, Cmd.none, env )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "element-panel" ]
        [ div [ class "typing-form" ]
            [ div [ class "typing-form__body" ]
                [ div [ class "typing-form__input" ]
                    [ text model.input ]
                ]
            ]
        , div
            [ class "wrap-lines" ]
            [ div [ class "wrap-line" ]
                [ span [ class "wrap-line__left" ] [ text "best: " ]
                , span [ class "wrap-line__right" ] [ text (kpmToString model.bestKPM) ]
                ]
            , div [ class "wrap-line" ]
                [ span [ class "wrap-line__left" ] [ text "kpm: " ]
                , span [ class "wrap-line__right" ] [ text (kpmToString model.nowKPM) ]
                ]
            ]
        ]



-- PROGRAM


kpmToString : Float -> String
kpmToString kpm =
    kpm |> Round.round 2


updateKPM : Time.Posix -> Model -> Model
updateKPM time model =
    let
        st =
            Time.posixToMillis model.lastTime |> toFloat

        lt =
            Time.posixToMillis time |> toFloat

        interval =
            lt - st

        keys =
            String.length model.input |> toFloat

        kpm =
            keys / (interval / (1000 * 60))

        newBestKPM =
            Basics.max kpm model.bestKPM
    in
    { model | bestKPM = newBestKPM, nowKPM = kpm, lastTime = time }



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ onKeyDown (D.map KeyDown keyDecoder)
        , Time.every 10000 TenSecond
        , Time.every 1000 OneSecond
        ]


keyDecoder : D.Decoder String
keyDecoder =
    D.field "key" D.string

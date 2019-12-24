module Page.TypeLongWord exposing (Model, Msg, init, subscriptions, update, view)

import API
import Browser.Events exposing (onKeyDown)
import Data.LongWord as LongWord exposing (Score)
import Env exposing (Env)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E
import Round
import Task
import Time
import Typing
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>), Parser, map, oneOf, s, top)
import Url.Parser.Query as Q



-- PROGRAM


type Route
    = Top (Maybe Int)


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Top (top <?> Q.int "id")
        ]



-- MODEL


type alias Model =
    { modelState : ModelState
    , typingState : TypingState
    , wordID : Maybe Int
    , inputHistory : String
    , typingData : Maybe Typing.Data
    , wordForView : String
    , miss : Int
    , missed : Bool
    , startTime : Time.Posix
    , finishTime : Time.Posix
    , bestScore : Maybe Score
    , scoreHistory : List Score
    , serverMSG : String
    }


type ModelState
    = Init
    | Loaded LongWord.LongWord
    | Error String


type TypingState
    = Waiting
    | Typing
    | Finish


init : Env -> ( Model, Cmd Msg )
init env =
    let
        url =
            env.url

        id =
            case
                Url.Parser.parse routeParser { url | path = "/" }
            of
                Just (Top d1) ->
                    d1

                Nothing ->
                    Nothing
    in
    ( Model
        Init
        Waiting
        id
        ""
        Nothing
        ""
        0
        False
        (Time.millisToPosix 0)
        (Time.millisToPosix 0)
        Nothing
        []
        ""
    , case id of
        Just wid ->
            API.getLongWord ReceiveLongWord wid

        Nothing ->
            API.getPresentLongWord ReceiveLongWord
    )


reset : Model -> Env -> ( Model, Cmd Msg, Env )
reset model env =
    case init env of
        ( nm, cmd ) ->
            ( { nm
                | bestScore = model.bestScore
                , scoreHistory = model.scoreHistory
                , modelState = model.modelState
                , typingData =
                    case model.modelState of
                        Init ->
                            Nothing

                        Error _ ->
                            Nothing

                        Loaded lw ->
                            Just (Typing.newData lw.wordForInput)
                , wordForView =
                    case model.modelState of
                        Init ->
                            ""

                        Error _ ->
                            ""

                        Loaded lw ->
                            lw.wordForView
              }
            , Cmd.none
            , env
            )



-- UPDATE


type Msg
    = ReceiveLongWord (Result Http.Error LongWord.LongWord)
    | KeyDown String
    | StartTime Time.Posix
    | FinishTime Time.Posix
    | DeleteWord
    | ReceiveDeleteWord (Result Http.Error String)
    | RegistRanking
    | ReceiveRegistRanking (Result Http.Error String)


update : Msg -> Model -> Env -> ( Model, Cmd Msg, Env )
update msg model env =
    case msg of
        ReceiveLongWord (Ok lw) ->
            ( { model
                | modelState = Loaded lw
                , typingData = Just (Typing.newData lw.wordForInput)
                , wordForView = lw.wordForView
              }
            , Cmd.none
            , env
            )

        ReceiveLongWord (Err e) ->
            ( { model | modelState = Error "お題取得失敗" }, Cmd.none, env )

        KeyDown key ->
            case ( model.modelState, model.typingData ) of
                ( Init, _ ) ->
                    ( model, Cmd.none, env )

                ( Error _, _ ) ->
                    ( model, Cmd.none, env )

                ( Loaded _, Nothing ) ->
                    ( model, Cmd.none, env )

                ( Loaded _, Just typingData ) ->
                    case key of
                        "Shift" ->
                            ( model, Cmd.none, env )

                        "Enter" ->
                            ( model, Cmd.none, env )

                        "Backspace" ->
                            ( model, Cmd.none, env )

                        "Control" ->
                            ( model, Cmd.none, env )

                        "Tab" ->
                            ( model, Cmd.none, env )

                        "Escape" ->
                            reset model env

                        _ ->
                            case model.typingState of
                                Waiting ->
                                    let
                                        stc =
                                            Task.perform StartTime Time.now
                                    in
                                    case Typing.typeTo key typingData of
                                        Nothing ->
                                            ( model, Cmd.none, env )

                                        Just d ->
                                            let
                                                st =
                                                    if d.restWords == "" then
                                                        Finish

                                                    else
                                                        Typing

                                                ftc =
                                                    if st == Finish then
                                                        Task.perform FinishTime Time.now

                                                    else
                                                        Cmd.none
                                            in
                                            ( { model
                                                | inputHistory = model.inputHistory ++ key
                                                , typingData = Just d
                                                , typingState = st
                                                , missed = False
                                              }
                                            , Cmd.batch [ ftc, stc ]
                                            , env
                                            )

                                Typing ->
                                    case Typing.typeTo key typingData of
                                        Nothing ->
                                            ( { model | miss = model.miss + 1, missed = True }, Cmd.none, env )

                                        Just d ->
                                            let
                                                st =
                                                    if d.restWords == "" then
                                                        Finish

                                                    else
                                                        Typing

                                                ftc =
                                                    if st == Finish then
                                                        Task.perform FinishTime Time.now

                                                    else
                                                        Cmd.none
                                            in
                                            ( { model
                                                | inputHistory = model.inputHistory ++ key
                                                , typingData = Just d
                                                , typingState = st
                                                , missed = False
                                              }
                                            , ftc
                                            , env
                                            )

                                Finish ->
                                    ( model, Cmd.none, env )

        StartTime time ->
            ( { model | startTime = time }, Cmd.none, env )

        FinishTime time ->
            let
                score =
                    { time = Time.posixToMillis time - Time.posixToMillis model.startTime
                    , keys = String.length model.inputHistory
                    , miss = model.miss
                    }

                su =
                    case model.bestScore of
                        Just bs ->
                            if bs.time > score.time then
                                True

                            else
                                False

                        Nothing ->
                            True
            in
            ( { model
                | finishTime = time
                , bestScore =
                    if su then
                        Just score

                    else
                        model.bestScore
                , scoreHistory = score :: model.scoreHistory
              }
            , Cmd.none
            , env
            )

        DeleteWord ->
            case model.wordID of
                Just wid ->
                    ( model, API.deleteLongWord ReceiveDeleteWord env.user wid, env )

                Nothing ->
                    ( model, Cmd.none, env )

        ReceiveDeleteWord (Ok str) ->
            ( { model | serverMSG = str }, Cmd.none, env )

        ReceiveDeleteWord (Err str) ->
            ( { model | serverMSG = "エラー" }, Cmd.none, env )

        RegistRanking ->
            case ( model.wordID, model.bestScore ) of
                ( Just wid, Just score ) ->
                    ( model, API.registLongWordRanking ReceiveRegistRanking env.user wid score, env )

                ( _, _ ) ->
                    ( model, Cmd.none, env )

        ReceiveRegistRanking (Ok str) ->
            ( { model | serverMSG = str }, Cmd.none, env )

        ReceiveRegistRanking (Err str) ->
            ( { model | serverMSG = "エラー" }, Cmd.none, env )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ class "element-panel" ]
            [ case ( model.modelState, model.typingData ) of
                ( Init, _ ) ->
                    div [] [ text "準備中" ]

                ( Error str, _ ) ->
                    div [] [ text ("エラー: " ++ str) ]

                ( Loaded _, Nothing ) ->
                    div [] [ text "データ解釈失敗" ]

                ( Loaded _, Just typingData ) ->
                    div []
                        [ div
                            [ class
                                (if model.missed then
                                    "typing-form__missed"

                                 else
                                    "typing-form"
                                )
                            ]
                            [ div [ class "typing-form__body" ]
                                [ div [ class "typing-form__words" ]
                                    [ span [ class "typing-form__fixed" ]
                                        [ text
                                            (String.left
                                                (String.length typingData.fixedWords)
                                                model.wordForView
                                                |> String.replace "ζ" ""
                                            )
                                        ]
                                    , span [ class "typing-form__rest " ]
                                        [ text
                                            (String.dropLeft
                                                (String.length typingData.fixedWords)
                                                model.wordForView
                                                |> String.replace "ζ" ""
                                            )
                                        ]
                                    ]
                                , div [ class "typing-form__input" ]
                                    [ text
                                        (if String.length model.inputHistory > 30 then
                                            String.right 30 model.inputHistory

                                         else
                                            model.inputHistory
                                        )
                                    ]
                                , div
                                    [ class
                                        (if model.typingState == Finish then
                                            "typing-form__state__finish"

                                         else
                                            "typing-form__state"
                                        )
                                    ]
                                    [ text
                                        ("ミス数: "
                                            ++ String.fromInt model.miss
                                            ++ (if model.typingState == Finish then
                                                    ", "
                                                        ++ calcAccuracy model.miss (String.length model.inputHistory)
                                                        ++ "%, "
                                                        ++ calcSec model.startTime model.finishTime
                                                        ++ "秒, "
                                                        ++ calcKpm model.startTime model.finishTime (String.length model.inputHistory)
                                                        ++ "打/分, "
                                                        ++ "    Finish"

                                                else
                                                    ""
                                               )
                                        )
                                    ]
                                ]
                            ]
                        , viewBestScore model.bestScore
                        , viewScoreHistory model.scoreHistory
                        ]
            ]
        , div [ class "element-panel" ]
            [ button [ onClick RegistRanking ] [ text "ランキング登録" ]
            , button [ onClick DeleteWord ] [ text "ワード削除" ]
            , div [] [ text model.serverMSG ]
            ]
        , div [ class "element-panel" ]
            []
        ]



---- SUBSCRIPTIONS ----


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ onKeyDown (D.map KeyDown keyDecoder)
        ]


keyDecoder : D.Decoder String
keyDecoder =
    D.field "key" D.string



---- PROGRAM ----


viewScore : Maybe Score -> Html msg
viewScore mscore =
    case mscore of
        Just score ->
            let
                time =
                    toFloat score.time / 1000

                ac =
                    (toFloat score.keys - toFloat score.miss) / toFloat score.keys * 100

                kpm =
                    toFloat score.keys / (time / 60)
            in
            div
                [ class "typing-score__body" ]
                [ text (Round.round 2 time ++ "秒, ")
                , text (String.fromInt score.miss ++ "ミス, ")
                , text (Round.round 2 ac ++ "%, ")
                , text (Round.round 2 kpm ++ "打/分")
                ]

        Nothing ->
            div [] []


viewBestScore : Maybe Score -> Html msg
viewBestScore mscore =
    div [ class "typing-score" ]
        [ div [ class "typing-score__title" ]
            [ text "自己ベスト" ]
        , viewScore mscore
        ]


viewScoreHistory : List Score -> Html msg
viewScoreHistory scoreList =
    let
        sl =
            List.map (\n -> li [] [ viewScore (Just n) ]) scoreList
    in
    div [ class "typing-score-history" ]
        [ div [ class "typing-score-history__title" ]
            [ text ("スコア履歴(" ++ String.fromInt (List.length sl) ++ ")") ]
        , ul [] sl
        ]


calcSec : Time.Posix -> Time.Posix -> String
calcSec startTime finishTime =
    let
        st =
            Time.posixToMillis startTime |> toFloat

        ft =
            Time.posixToMillis finishTime |> toFloat

        time =
            (ft - st) / 1000
    in
    Round.round 2 time


calcKpm : Time.Posix -> Time.Posix -> Int -> String
calcKpm startTime finishTime keys =
    let
        st =
            Time.posixToMillis startTime |> toFloat

        ft =
            Time.posixToMillis finishTime |> toFloat

        time =
            ft - st

        kpm =
            toFloat keys / (time / (1000 * 60))
    in
    Round.round 2 kpm


calcAccuracy : Int -> Int -> String
calcAccuracy miss keys =
    let
        ac =
            (toFloat keys - toFloat miss) / toFloat keys * 100
    in
    Round.round 2 ac

module Page.TypeShortWord exposing (Model, Msg, init, initInTrial, subscriptions, update, view, viewTyping)

import API
import Browser.Events exposing (onKeyDown)
import CountDown
import Data.ShortWord as ShortWord exposing (Score)
import Env exposing (Env)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E
import Random
import Random.List as RandomList
import Round
import Task
import Time
import Typing2 as Typing
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
        [ Url.Parser.map Top (top <?> Q.int "id")
        ]



-- MODEL


type alias Model =
    { state : State
    , notice : String
    }


type State
    = Init
    | Ready ReadyData
    | Error String


type alias ReadyData =
    { state : ReadyState
    , shortWords : ShortWord.ShortWords
    , customTypingWords : CustomTypingWords
    , countDownTimer : CountDown.Timer
    , bestScore : Maybe Score
    , scoreHistory : List Score
    , ranking : Maybe API.LongWordRanking
    }


type ReadyState
    = Play
    | Trial


type alias CustomTypingWords =
    { finish : List CustomTypingWord
    , rest : List CustomTypingWord
    }


type alias CustomTypingWord =
    { index : Int
    , typingData : Typing.Data
    , inputHistory : String
    , miss : Int
    , startTime : Time.Posix
    , finishTime : Time.Posix
    }


initialCountDownTimer : CountDown.Timer
initialCountDownTimer =
    CountDown.init False 3000 1000


getCountDownTimer : Model -> Maybe CountDown.Timer
getCountDownTimer model =
    case model.state of
        Init ->
            Nothing

        Error _ ->
            Nothing

        Ready readyData ->
            Just readyData.countDownTimer


setCountDownTimer : CountDown.Timer -> Model -> Model
setCountDownTimer timer model =
    case model.state of
        Init ->
            model

        Error _ ->
            model

        Ready readyData ->
            { model | state = Ready { readyData | countDownTimer = timer } }


startCountDownTimer : Model -> Model
startCountDownTimer model =
    case getCountDownTimer model of
        Nothing ->
            model

        Just timer ->
            setCountDownTimer (CountDown.setStart timer) model


testDataOfShowtWords =
    { title = "title"
    , id = -1
    , words = [ ShortWord.Word "hoge" "hoge", ShortWord.Word "piyo" "piyo", ShortWord.Word "bar" "bar" ]
    }


newTestReadyData : ReadyData
newTestReadyData =
    ReadyData Trial testDataOfShowtWords (initCustomTypingWords testDataOfShowtWords) initialCountDownTimer Nothing [] Nothing


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
    ( Model (Ready newTestReadyData) "", Cmd.none )


initInTrial : ShortWord.ShortWords -> ( Model, Cmd Msg )
initInTrial words =
    ( Model Init "", Cmd.none )


reset : Model -> Model
reset model =
    case model.state of
        Init ->
            model

        Error _ ->
            model

        Ready readyData ->
            { model
                | state =
                    Ready
                        { readyData
                            | countDownTimer = initialCountDownTimer
                            , customTypingWords = initCustomTypingWords readyData.shortWords
                        }
            }


initCustomTypingWords : ShortWord.ShortWords -> CustomTypingWords
initCustomTypingWords sw =
    let
        isw =
            List.indexedMap Tuple.pair sw.words

        fun ( i, d ) =
            { index = i
            , typingData = Typing.newData d.wordForInput
            , inputHistory = ""
            , miss = 0
            , startTime = Time.millisToPosix 0
            , finishTime = Time.millisToPosix 0
            }
    in
    { finish = [], rest = List.map fun isw }



-- UPDATE


type Msg
    = ReceiveShortWords (Result Http.Error ShortWord.ShortWords)
    | KeyDown String
    | StartTime Time.Posix
    | FinishTime Time.Posix
    | DeleteWord
    | ReceiveDeleteWord (Result Http.Error String)
    | RegistRanking
    | ReceiveRegistRanking (Result Http.Error String)
    | GetRanking
    | ReceiveGetRanking (Result Http.Error API.LongWordRanking)
    | ResetGame (List ShortWord.Word)
    | Tick CountDown.Timer


run : msg -> Cmd msg
run m =
    Task.perform (always m) (Task.succeed ())


getCurrentWord : CustomTypingWords -> Maybe CustomTypingWord
getCurrentWord words =
    case List.head words.rest of
        Nothing ->
            Nothing

        Just customTypingWord ->
            Just customTypingWord


updateCurrentWord : CustomTypingWord -> CustomTypingWords -> CustomTypingWords
updateCurrentWord newWord words =
    let
        newRest =
            case words.rest of
                a :: b ->
                    newWord :: b

                [] ->
                    []
    in
    { words | rest = newRest }


updateByCorrect : String -> Typing.Data -> CustomTypingWord -> CustomTypingWords -> CustomTypingWords
updateByCorrect key newTypingData word words =
    let
        newWord =
            { word
                | typingData = newTypingData
                , inputHistory = word.inputHistory ++ key
            }
    in
    updateCurrentWord newWord words


updateByMiss : CustomTypingWord -> CustomTypingWords -> CustomTypingWords
updateByMiss word words =
    let
        newWord =
            { word
                | miss = word.miss + 1
            }
    in
    updateCurrentWord newWord words


suffleWords : ReadyData -> Cmd Msg
suffleWords readyData =
    readyData.shortWords.words
        |> RandomList.shuffle
        |> Random.map (List.take 1)
        |> Random.generate ResetGame


update : Msg -> Model -> Env -> ( Model, Cmd Msg, Env )
update msg model env =
    case msg of
        ReceiveShortWords (Ok sw) ->
            ( model, Cmd.none, env )

        ReceiveShortWords (Err e) ->
            ( { model | state = Error "お題取得失敗" }, Cmd.none, env )

        ResetGame list ->
            let
                hoge =
                    Debug.log "hoge" list
            in
            ( model, Cmd.none, env )

        KeyDown key ->
            case model.state of
                Init ->
                    ( model, Cmd.none, env )

                Error _ ->
                    ( model, Cmd.none, env )

                Ready readyData ->
                    case ( CountDown.isZero readyData.countDownTimer, key ) of
                        ( _, "Escape" ) ->
                            ( reset model
                            , suffleWords readyData
                            , env
                            )

                        ( False, " " ) ->
                            ( startCountDownTimer model, Cmd.none, env )

                        ( False, _ ) ->
                            ( model, Cmd.none, env )

                        ( True, _ ) ->
                            let
                                ( newCustomTypingWords, resCmd ) =
                                    case getCurrentWord readyData.customTypingWords of
                                        Nothing ->
                                            ( readyData.customTypingWords, Cmd.none )

                                        Just customTypingWord ->
                                            let
                                                nowT =
                                                    customTypingWord.typingData

                                                nowS =
                                                    nowT |> Typing.toState

                                                newT =
                                                    Typing.typeTo key nowT

                                                newS =
                                                    newT |> Typing.toState
                                            in
                                            case nowS of
                                                Typing.Waiting ->
                                                    case newS of
                                                        Typing.Waiting ->
                                                            -- Waitingにはならないのに
                                                            ( readyData.customTypingWords, Cmd.none )

                                                        Typing.Typing ->
                                                            -- タイピングが開始された
                                                            ( updateByCorrect key newT customTypingWord readyData.customTypingWords, Task.perform StartTime Time.now )

                                                        Typing.Miss ->
                                                            ( updateByMiss customTypingWord readyData.customTypingWords, Task.perform StartTime Time.now )

                                                        Typing.Finish ->
                                                            -- finishTimeとstartTimeが同じ値で初期化されているなら、何もしなければfinishTime-startTime == 0になってくれる
                                                            ( updateByCorrect key newT customTypingWord readyData.customTypingWords, Cmd.none )

                                                Typing.Typing ->
                                                    case newS of
                                                        Typing.Waiting ->
                                                            -- Waitingにはならないのに
                                                            ( readyData.customTypingWords, Cmd.none )

                                                        Typing.Typing ->
                                                            ( updateByCorrect key newT customTypingWord readyData.customTypingWords, Cmd.none )

                                                        Typing.Miss ->
                                                            ( updateByMiss customTypingWord readyData.customTypingWords, Cmd.none )

                                                        Typing.Finish ->
                                                            ( updateByCorrect key newT customTypingWord readyData.customTypingWords, Task.perform FinishTime Time.now )

                                                Typing.Miss ->
                                                    case newS of
                                                        Typing.Waiting ->
                                                            -- Waitingにはならないのに
                                                            ( readyData.customTypingWords, Cmd.none )

                                                        Typing.Typing ->
                                                            ( updateByCorrect key newT customTypingWord readyData.customTypingWords, Cmd.none )

                                                        Typing.Miss ->
                                                            ( updateByMiss customTypingWord readyData.customTypingWords, Cmd.none )

                                                        Typing.Finish ->
                                                            ( updateByCorrect key newT customTypingWord readyData.customTypingWords, Task.perform FinishTime Time.now )

                                                Typing.Finish ->
                                                    case newS of
                                                        _ ->
                                                            ( readyData.customTypingWords, Cmd.none )
                            in
                            ( { model | state = Ready { readyData | customTypingWords = newCustomTypingWords } }, resCmd, env )

        StartTime time ->
            case model.state of
                Init ->
                    ( model, Cmd.none, env )

                Error _ ->
                    ( model, Cmd.none, env )

                Ready readyData ->
                    case getCurrentWord readyData.customTypingWords of
                        Nothing ->
                            ( model, Cmd.none, env )

                        Just word ->
                            let
                                newWord =
                                    { word | startTime = time }

                                newWords =
                                    updateCurrentWord newWord readyData.customTypingWords
                            in
                            ( { model
                                | state = Ready { readyData | customTypingWords = newWords }
                              }
                            , Cmd.none
                            , env
                            )

        FinishTime time ->
            case model.state of
                Init ->
                    ( model, Cmd.none, env )

                Error _ ->
                    ( model, Cmd.none, env )

                Ready readyData ->
                    case getCurrentWord readyData.customTypingWords of
                        Nothing ->
                            ( model, Cmd.none, env )

                        Just word ->
                            let
                                newWord =
                                    { word | finishTime = time }

                                pWords =
                                    readyData.customTypingWords

                                newWords =
                                    { pWords
                                        | finish = pWords.finish ++ [ newWord ]
                                        , rest = List.drop 1 pWords.rest
                                    }
                            in
                            ( { model | state = Ready { readyData | customTypingWords = newWords } }, Cmd.none, env )

        DeleteWord ->
            ( model, Cmd.none, env )

        ReceiveDeleteWord (Ok str) ->
            ( { model | notice = str }, Cmd.none, env )

        ReceiveDeleteWord (Err str) ->
            ( { model | notice = "エラー" }, Cmd.none, env )

        RegistRanking ->
            ( model, Cmd.none, env )

        ReceiveRegistRanking (Ok str) ->
            ( { model | notice = str }, Cmd.none, env )

        ReceiveRegistRanking (Err str) ->
            ( { model | notice = "エラー" }, Cmd.none, env )

        GetRanking ->
            ( model, Cmd.none, env )

        ReceiveGetRanking (Ok lwr) ->
            ( model, Cmd.none, env )

        ReceiveGetRanking (Err e) ->
            ( { model | notice = "ランキング取得失敗" }, Cmd.none, env )

        Tick newTimer ->
            ( setCountDownTimer newTimer model, Cmd.none, env )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ class "element-panel" ]
            [ viewTyping model ]
        , div [ class "element-panel" ]
            [ getCountDownTimer model
                |> Maybe.map CountDown.getSecond
                |> Maybe.map String.fromInt
                |> Maybe.withDefault "error"
                |> text
            ]
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (D.map KeyDown keyDecoder)
        , case getCountDownTimer model of
            Nothing ->
                Sub.none

            Just timer ->
                CountDown.treatSubscription timer Tick
        ]


keyDecoder : D.Decoder String
keyDecoder =
    D.field "key" D.string



---- PROGRAM ----


type alias UsefulScore =
    { time : String
    , miss : String
    , accuracy : String
    , kpm : String
    }


scoreToUsefulScore : Score -> UsefulScore
scoreToUsefulScore score =
    let
        time =
            toFloat score.time / 1000

        ac =
            (toFloat score.keys - toFloat score.miss) / toFloat score.keys * 100

        kpm =
            toFloat score.keys / (time / 60)
    in
    { time = Round.round 2 time
    , miss = String.fromInt score.miss
    , accuracy = Round.round 2 ac
    , kpm = Round.round 2 kpm
    }


viewRanking : API.LongWordRankingScore -> Html msg
viewRanking score =
    let
        us =
            scoreToUsefulScore score.score
    in
    tr
        [ class
            (if score.your then
                "ranking-table__you"

             else
                ""
            )
        ]
        [ td [] [ text (String.fromInt score.rank) ]
        , td [] [ text score.name ]
        , td [] [ text us.time ]
        , td [] [ text us.miss ]
        , td [] [ text us.accuracy ]
        , td [] [ text us.kpm ]
        ]


viewRankingList : Maybe API.LongWordRanking -> Html msg
viewRankingList lwr =
    case lwr of
        Nothing ->
            div [] []

        Just ranking ->
            let
                rl =
                    List.map (\n -> viewRanking n) ranking
            in
            table [ class "ranking-table" ]
                [ thead []
                    [ th [] [ text "順位" ]
                    , th [] [ text "名前" ]
                    , th [] [ text "タイム" ]
                    , th [] [ text "miss" ]
                    , th [] [ text "正確率" ]
                    , th [] [ text "kpm" ]
                    ]
                , tbody []
                    rl
                ]


viewScore : Maybe Score -> Html msg
viewScore mscore =
    case mscore of
        Just score ->
            let
                us =
                    scoreToUsefulScore score
            in
            div
                [ class "typing-score__body" ]
                [ text (us.time ++ "秒, ")
                , text (us.miss ++ "ミス, ")
                , text (us.accuracy ++ "%, ")
                , text (us.kpm ++ "打/分")
                ]

        Nothing ->
            div [] []


viewTyping : Model -> Html msg
viewTyping model =
    case model.state of
        Init ->
            div [] [ text "init viewTyping" ]

        Error str ->
            div [] [ text ("error(" ++ str ++ " viewTyping") ]

        Ready readyData ->
            div [] [ viewText readyData.customTypingWords ]


viewText : CustomTypingWords -> Html msg
viewText words =
    case List.head words.rest of
        Just word ->
            div []
                [ text "viewText"
                , br [] []
                , text (Typing.getFixed word.typingData)
                , br [] []
                , text (Typing.getRest word.typingData)
                ]

        Nothing ->
            div [] [ text "finish viewText" ]


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

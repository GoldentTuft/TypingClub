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
    , miss : Int
    , missed : Bool
    }


type alias CustomTypingWord =
    { index : Int
    , typingData : Typing.Data
    , wordForView : String
    , inputHistory : String
    , miss : Int
    , rhythmTimer : CountDown.Timer
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


getRhythmTimer : Model -> Maybe CountDown.Timer
getRhythmTimer model =
    case model.state of
        Init ->
            Nothing

        Error _ ->
            Nothing

        Ready readyData ->
            case getCurrentWord readyData.customTypingWords of
                Nothing ->
                    Nothing

                Just word ->
                    Just word.rhythmTimer


setCountDownTimer : CountDown.Timer -> Model -> Model
setCountDownTimer timer model =
    case model.state of
        Init ->
            model

        Error _ ->
            model

        Ready readyData ->
            { model | state = Ready { readyData | countDownTimer = timer } }


setRhythmTimer : CountDown.Timer -> Model -> Model
setRhythmTimer timer model =
    case model.state of
        Init ->
            model

        Error _ ->
            model

        Ready readyData ->
            case getCurrentWord readyData.customTypingWords of
                Nothing ->
                    model

                Just word ->
                    let
                        newWord =
                            { word | rhythmTimer = timer }

                        newWords =
                            updateCurrentWord newWord readyData.customTypingWords
                    in
                    { model | state = Ready { readyData | customTypingWords = newWords } }


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
    , words = [ ShortWord.Word "田ζ中さん" "たなかさん", ShortWord.Word "佐ζ藤さん" "さとうさん", ShortWord.Word "bar" "bar" ]
    }


newTestReadyData : ReadyData
newTestReadyData =
    ReadyData Trial testDataOfShowtWords (initCustomTypingWords testDataOfShowtWords.words) initialCountDownTimer Nothing [] Nothing


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

        readyData =
            newTestReadyData
    in
    ( Model (Ready readyData) "", shuffleWords readyData.shortWords )


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
                            , customTypingWords = initCustomTypingWords readyData.shortWords.words
                        }
            }


initCustomTypingWords : List ShortWord.Word -> CustomTypingWords
initCustomTypingWords words =
    let
        isw =
            List.indexedMap Tuple.pair words

        fun ( i, d ) =
            { index = i
            , typingData = Typing.newData d.wordForInput
            , wordForView = d.wordForView
            , inputHistory = ""
            , miss = 0
            , rhythmTimer =
                if i == 0 then
                    CountDown.init True 1 1

                else
                    CountDown.init True 500 100
            , startTime = Time.millisToPosix 0
            , finishTime = Time.millisToPosix 0
            }
    in
    { finish = [], rest = List.map fun isw, miss = 0, missed = False }



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
    | ShuffleWords (List ShortWord.Word)
    | Tick CountDown.Timer
    | TickRhythm CountDown.Timer


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

        newWords =
            { words
                | missed = False
            }
    in
    updateCurrentWord newWord newWords


updateByMiss : CustomTypingWord -> CustomTypingWords -> CustomTypingWords
updateByMiss word words =
    let
        newWord =
            { word
                | miss = word.miss + 1
            }

        newWords =
            { words
                | miss = words.miss + 1
                , missed = True
            }
    in
    updateCurrentWord newWord newWords


shuffleWords : ShortWord.ShortWords -> Cmd Msg
shuffleWords shortWords =
    shortWords.words
        |> RandomList.shuffle
        |> Random.map (List.take (Basics.min 20 (List.length shortWords.words)))
        |> Random.generate ShuffleWords


update : Msg -> Model -> Env -> ( Model, Cmd Msg, Env )
update msg model env =
    case msg of
        ReceiveShortWords (Ok sw) ->
            ( model, Cmd.none, env )

        ReceiveShortWords (Err e) ->
            ( { model | state = Error "お題取得失敗" }, Cmd.none, env )

        ShuffleWords list ->
            case model.state of
                Init ->
                    ( model, Cmd.none, env )

                Error _ ->
                    ( model, Cmd.none, env )

                Ready readyData ->
                    ( { model
                        | state =
                            Ready
                                { readyData
                                    | customTypingWords = initCustomTypingWords list
                                }
                      }
                    , Cmd.none
                    , env
                    )

        KeyDown key ->
            case model.state of
                Init ->
                    ( model, Cmd.none, env )

                Error _ ->
                    ( model, Cmd.none, env )

                Ready readyData ->
                    case ( CountDown.getState readyData.countDownTimer == CountDown.Zero, key ) of
                        ( _, "Escape" ) ->
                            ( reset model
                            , shuffleWords readyData.shortWords
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
                                            case CountDown.getState customTypingWord.rhythmTimer == CountDown.Zero of
                                                False ->
                                                    -- 入力ミスとしてもいいかも
                                                    ( readyData.customTypingWords, Cmd.none )

                                                True ->
                                                    let
                                                        nowT =
                                                            customTypingWord.typingData

                                                        nowS =
                                                            nowT |> Typing.getState

                                                        newT =
                                                            Typing.typeTo key nowT

                                                        newS =
                                                            newT |> Typing.getState
                                                    in
                                                    case nowS of
                                                        Typing.Waiting ->
                                                            case newS of
                                                                Typing.Waiting ->
                                                                    -- Waitingにはならないのに
                                                                    ( readyData.customTypingWords, Cmd.none )

                                                                Typing.Typing ->
                                                                    -- タイピングが開始された
                                                                    ( updateByCorrect key newT customTypingWord readyData.customTypingWords, Cmd.none )

                                                                Typing.Miss ->
                                                                    ( updateByMiss customTypingWord readyData.customTypingWords, Cmd.none )

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

        TickRhythm newTimer ->
            ( setRhythmTimer newTimer model
            , case CountDown.getState newTimer of
                CountDown.Zero ->
                    Task.perform StartTime Time.now

                _ ->
                    Cmd.none
            , env
            )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ class "element-panel" ]
            [ case getCountDownTimer model of
                Nothing ->
                    text model.notice

                Just timer ->
                    case CountDown.getState timer of
                        CountDown.Stop ->
                            text "カウントダウンストップエラー"

                        CountDown.Error ->
                            text "カウントダウンエラー"

                        CountDown.Ready ->
                            text "スペースキーでスタートです"

                        CountDown.Tick ->
                            CountDown.getSecond timer
                                |> String.fromInt
                                |> text

                        CountDown.Zero ->
                            viewTyping model
            ]
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
    let
        ct =
            getCountDownTimer model

        rt =
            getRhythmTimer model
    in
    Sub.batch
        [ onKeyDown (D.map KeyDown keyDecoder)
        , case ct of
            Nothing ->
                Sub.none

            Just jct ->
                case ( CountDown.getState jct, rt ) of
                    ( CountDown.Zero, Just jrt ) ->
                        CountDown.treatSubscription jrt TickRhythm

                    ( _, _ ) ->
                        CountDown.treatSubscription jct Tick
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



-- viewDetails : Maybe Score -> Html msg
-- viewDetails mscore =
--     case mscore of
--         Just score ->
--             let
--                 us =
--                     scoreToUsefulScore score
--             in
--             div
--                 [ class "typing-score__body" ]
--                 [ text (us.time ++ "秒, ")
--                 , text (us.miss ++ "ミス, ")
--                 , text (us.accuracy ++ "%, ")
--                 , text (us.kpm ++ "打/分")
--                 ]
--
--         Nothing ->
--             div [] []


viewTyping : Model -> Html Msg
viewTyping model =
    case model.state of
        Init ->
            div [] [ text "init viewTyping" ]

        Error str ->
            div [] [ text ("error(" ++ str ++ " viewTyping") ]

        Ready readyData ->
            case getCurrentWord readyData.customTypingWords of
                Nothing ->
                    -- Finishのはず
                    viewDetails readyData.customTypingWords

                Just word ->
                    viewText readyData.customTypingWords word


getSumOfInput : List CustomTypingWord -> Int
getSumOfInput words =
    let
        f w sum =
            sum + String.length w.inputHistory
    in
    List.foldl f 0 words


getSumOfMillis : List CustomTypingWord -> Int
getSumOfMillis words =
    let
        f w sum =
            sum + (Time.posixToMillis w.finishTime - Time.posixToMillis w.startTime)
    in
    List.foldl f 0 words


viewAccuracy : Float -> Html Msg
viewAccuracy ac =
    text ("正確率: " ++ (ac * 100 |> round2) ++ "%")


viewScore : Float -> Html Msg
viewScore score =
    text ("score: " ++ (score |> Round.round 0) ++ "pt")


nbsp =
    "\u{00A0}"


viewDetails : CustomTypingWords -> Html Msg
viewDetails words =
    let
        keys =
            getSumOfInput words.finish

        millis =
            getSumOfMillis words.finish
    in
    div []
        [ viewKpm (calcKpm millis keys)
        , text ", "
        , viewAccuracy (calcAccuracy words.miss keys)
        , text ", "
        , text (String.fromInt millis)
        , text ", "
        , text ("miss: " ++ String.fromInt words.miss)
        , text ", "
        , viewScore (calcScore millis keys words.miss)
        ]


getFixed : String -> Typing.Data -> String
getFixed wordForView td =
    let
        fixed =
            Typing.getFixed td
    in
    wordForView
        |> String.left (String.length fixed)
        |> String.replace "ζ" ""


getRest : String -> Typing.Data -> String
getRest wordForView td =
    let
        fixed =
            Typing.getFixed td
    in
    wordForView
        |> String.dropLeft (String.length fixed)
        |> String.replace "ζ" ""


getInputHistory : CustomTypingWord -> String
getInputHistory word =
    if String.length word.inputHistory > 30 then
        String.right 30 word.inputHistory

    else
        word.inputHistory


viewText : CustomTypingWords -> CustomTypingWord -> Html Msg
viewText words word =
    div
        [ class
            (case words.missed of
                True ->
                    "typing-form__missed"

                False ->
                    "typing-form"
            )
        ]
        [ div [ class "typing-form__body" ]
            [ div
                [ class "typing-form__words"
                , style
                    "visibility"
                    (case CountDown.getState word.rhythmTimer of
                        CountDown.Zero ->
                            "visible"

                        _ ->
                            "hidden"
                    )
                ]
                [ span [ class "typing-form__fixed" ]
                    [ text (getFixed word.wordForView word.typingData) ]
                , span [ class "typing-form__rest" ]
                    [ text (getRest word.wordForView word.typingData) ]
                ]
            , div [ class "typing-form__input" ]
                [ text (getInputHistory word) ]
            , div [ class "typing-form__state" ]
                [ text ("ミス数:" ++ String.fromInt words.miss) ]
            ]
        ]



-- viewBestScore : Maybe Score -> Html Msg
-- viewBestScore mscore =
--     div [ class "typing-score" ]
--         [ div [ class "typing-score__title" ]
--             [ text "自己ベスト" ]
--         , viewScore mscore
--         ]
-- viewScoreHistory : List Score -> Html Msg
-- viewScoreHistory scoreList =
--     let
--         sl =
--             List.map (\n -> li [] [ viewScore (Just n) ]) scoreList
--     in
--     div [ class "typing-score-history" ]
--         [ div [ class "typing-score-history__title" ]
--             [ text ("スコア履歴(" ++ String.fromInt (List.length sl) ++ ")") ]
--         , ul [] sl
--         ]


viewKpm : Float -> Html Msg
viewKpm kpm =
    text ("kpm: " ++ (kpm |> round2))


calcScore : Int -> Int -> Int -> Float
calcScore millis keys miss =
    -- 3ミスで1秒追加
    toFloat keys / ((toFloat millis + toFloat miss / 3 * 1000) / (1000 * 60))


calcKpm : Int -> Int -> Float
calcKpm millis keys =
    toFloat keys / (toFloat millis / (1000 * 60))


calcAccuracy : Int -> Int -> Float
calcAccuracy miss keys =
    toFloat keys / (toFloat keys + toFloat miss)


round2 : Float -> String
round2 a =
    Round.round 2 a

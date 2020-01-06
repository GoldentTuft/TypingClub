module Page.PostLongWord exposing (Model, Msg, init, subscriptions, update, view)

import API
import Browser.Events exposing (onKeyDown)
import Data.LongWord as LongWord
import Env exposing (Env)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Page.TypeLongWord
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>), Parser, map, oneOf, s, top)
import Url.Parser.Query as Q



-- MODEL


type alias Model =
    { state : State
    , lw : LongWord.LongWord
    }


type State
    = Edit EditState
    | Trial Page.TypeLongWord.Model


type EditState
    = Init
    | Waiting String
    | Success String
    | Error String


init : Env -> ( Model, Cmd Msg )
init env =
    ( Model (Edit Init) LongWord.new
    , Cmd.none
    )



-- UPDATE


type Msg
    = InputTitle String
    | InputWordForInput String
    | InputWordForView String
    | Post
    | ReceivePost (Result Http.Error String)
    | TryType
    | BackEdit
    | TypeLongWordMsg Page.TypeLongWord.Msg


update : Msg -> Model -> Env -> ( Model, Cmd Msg, Env )
update msg model env =
    let
        pLW =
            model.lw
    in
    case msg of
        InputTitle newInput ->
            ( { model | lw = { pLW | title = newInput } }, Cmd.none, env )

        InputWordForInput newInput ->
            ( { model | lw = { pLW | wordForInput = newInput } }, Cmd.none, env )

        InputWordForView newInput ->
            ( { model | lw = { pLW | wordForView = newInput } }, Cmd.none, env )

        Post ->
            ( { model | state = Edit (Waiting "登録中") }, API.postLongWord ReceivePost env.user model.lw, env )

        ReceivePost (Ok res) ->
            let
                st =
                    case res of
                        "登録成功" ->
                            Edit (Success res)

                        _ ->
                            Edit (Error ("登録失敗: " ++ res))
            in
            ( { model | state = st }, Cmd.none, env )

        ReceivePost (Err e) ->
            -- サーバーが文字列しか介さないから呼ばれない
            ( model, Cmd.none, env )

        TryType ->
            let
                ( pageModel, _ ) =
                    Page.TypeLongWord.initInTrial model.lw.wordForView model.lw.wordForInput
            in
            ( { model | state = Trial pageModel }, Cmd.none, env )

        BackEdit ->
            ( { model | state = Edit Init }, Cmd.none, env )

        TypeLongWordMsg subMsg ->
            case model.state of
                Trial subModel ->
                    let
                        ( newModel, topCmd, newEnv ) =
                            Page.TypeLongWord.update subMsg subModel env
                    in
                    ( { model | state = Trial newModel }
                    , Cmd.map TypeLongWordMsg topCmd
                    , newEnv
                    )

                _ ->
                    ( model, Cmd.none, env )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [ class "element-panel" ]
            (case model.state of
                Edit es ->
                    [ viewPostForm model ]

                Trial subModel ->
                    [ Page.TypeLongWord.viewTyping subModel
                    , button [ onClick BackEdit ] [ text "編集に戻る" ]
                    ]
            )
        , div [ class "element-panel" ]
            [ div [ class "normal-sentence" ]
                [ h3 []
                    [ text "入力用ワードについて" ]
                , div
                    []
                    [ text "英数字記号などは半角で、それ以外はひらがなでお願いします。「、」「。」『「」』はそのままでもOKです。"
                    , br [] []
                    , text "例: じぶん[さんきゅー.bye]"
                    ]
                ]
            , div [ class "normal-sentence" ]
                [ h3 []
                    [ text "表示用ワードについて" ]
                , div
                    []
                    [ text "漢字のように1文字が複数文字を表す場合、\"ζ\"を前に置き文字数を合わせてください。"
                    , br [] []
                    , text "例: 自ζ分「サンキュー。bye」"
                    ]
                ]
            , div [ class "normal-sentence" ]
                [ h3 []
                    [ text "補足" ]
                , div
                    []
                    [ text "1文字目の\"じ\"が\"自\"に相当し、2文字目の\"ぶ\"が\"ζ\"に、3文字目の\"ん\"が\"分\"に、といった具合です。"
                    , br [] []
                    , text "例: \"わたし\"=\"ζζ私\""
                    , br [] []
                    , text "例: \"じ\"=\"自\""
                    , br [] []
                    , text "入力用ワードと表示用ワードで文字数が一致しない場合は投稿ボタンが押せません。"
                    , br [] []
                    , text "投稿したお題はタイピングページで後で削除することができます。"
                    ]
                ]
            ]
        ]


viewPostForm : Model -> Html Msg
viewPostForm model =
    div []
        [ div []
            [ text "お題名"
            , br [] []
            , input
                [ onInput InputTitle
                , autofocus True
                , placeholder "お題名"
                , value model.lw.title
                ]
                []
            ]
        , div []
            [ text "入力用ワード"
            , br [] []
            , textarea
                [ onInput InputWordForInput
                , placeholder "入力用ワード"
                , value model.lw.wordForInput
                , cols 60
                , rows 10
                ]
                []
            ]
        , div []
            [ text "表示用ワード"
            , br [] []
            , textarea
                [ onInput InputWordForView
                , placeholder "表示用ワード"
                , value model.lw.wordForView
                , cols 60
                , rows 10
                ]
                []
            ]
        , button
            [ disabled (invalidRegistButton model), onClick TryType ]
            [ text "テストプレイ" ]
        , button
            [ disabled (invalidRegistButton model), onClick Post ]
            [ text "投稿" ]
        , viewState model
        ]


invalidRegistButton : Model -> Bool
invalidRegistButton model =
    let
        c1 =
            case model.state of
                Edit (Waiting _) ->
                    True

                Edit (Success _) ->
                    True

                _ ->
                    False

        c2 =
            not (LongWord.validate model.lw)
    in
    c1 || c2


viewState : Model -> Html Msg
viewState model =
    case model.state of
        Edit Init ->
            div []
                [ text "" ]

        Edit (Waiting str) ->
            div [] [ text str ]

        Edit (Error str) ->
            div [] [ text str ]

        Edit (Success str) ->
            div [] [ text str ]

        Trial _ ->
            div [] [ text "" ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        s1 =
            case model.state of
                Trial subModel ->
                    Sub.map TypeLongWordMsg Page.TypeLongWord.subscriptions

                _ ->
                    Sub.none
    in
    Sub.batch [ s1 ]

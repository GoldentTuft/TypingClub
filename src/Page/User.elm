module Page.User exposing (Model, Msg, init, subscriptions, update, view)

import API
import Data.User as User
import Env exposing (Env)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import Ports
import Url exposing (Url)
import Url.Builder
import Url.Parser exposing ((</>), (<?>), Parser, map, oneOf, s, top)
import Url.Parser.Query as Q



-- Program
-- MODEL


type alias Model =
    { state : State
    , user : User.User
    }


type State
    = Init
    | Success String
    | Error String
    | Waiting String


init : Env -> ( Model, Cmd Msg )
init env =
    ( Model Init env.user
    , Cmd.none
    )



-- UPDATE


type Msg
    = InputName String
    | InputID String
    | ReceiveNewUserID (Result Http.Error User.User)
    | GetNewUserID
    | ReName
    | ReceiveReName (Result Http.Error User.User)
    | ChangeUser
    | ReceiveChangeUser (Result Http.Error User.User)


update : Msg -> Model -> Env -> ( Model, Cmd Msg, Env )
update msg model env =
    let
        pUser =
            model.user
    in
    case msg of
        InputName name ->
            ( { model | user = { pUser | name = name } }, Cmd.none, env )

        InputID id ->
            ( { model | user = { pUser | id = id } }, Cmd.none, env )

        GetNewUserID ->
            ( { model | state = Waiting "取得中" }, API.getNewUserID ReceiveNewUserID, env )

        ReceiveNewUserID (Err e) ->
            ( { model | state = Error "取得失敗" }, Cmd.none, env )

        ReceiveNewUserID (Ok user) ->
            let
                newEnv =
                    { env | user = user }
            in
            ( { model | user = user, state = Success "新ID取得完了" }, Ports.saveUser (User.encodeUser user), newEnv )

        ReName ->
            ( { model | state = Waiting "名前変更中" }, API.reName ReceiveReName model.user, env )

        ReceiveReName (Ok user) ->
            ( { model | user = user, state = Success "名前変更完了" }, Ports.saveUser (User.encodeUser user), { env | user = user } )

        ReceiveReName (Err e) ->
            ( { model | state = Error "名前変更失敗" }, Cmd.none, env )

        ChangeUser ->
            ( { model | state = Waiting "ID切り替え中" }, API.changeUser ReceiveChangeUser model.user, env )

        ReceiveChangeUser (Ok user) ->
            ( { model | user = user, state = Success "ID切り替え完了" }, Ports.saveUser (User.encodeUser user), { env | user = user } )

        ReceiveChangeUser (Err e) ->
            ( { model | state = Error "ID切り替え失敗" }, Cmd.none, env )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "element-panel" ]
        [ div []
            [ text "ユーザー名"
            , br [] []
            , input
                [ onInput InputName
                , autofocus False
                , value model.user.name
                ]
                []
            , br [] []
            , button
                [ disabled (String.isEmpty model.user.name), onClick ReName ]
                [ text "変更" ]
            ]
        , div []
            [ text "ユーザーID"
            , br [] []
            , input
                [ onInput InputID
                , autofocus False
                , value model.user.id
                ]
                []
            , br [] []
            , button
                [ disabled (String.isEmpty model.user.id), onClick ChangeUser ]
                [ text "切り替え" ]
            , button
                [ onClick GetNewUserID ]
                [ text "再取得" ]
            ]
        , viewState model
        ]


viewState : Model -> Html Msg
viewState model =
    div []
        [ case model.state of
            Init ->
                text ""

            Success str ->
                div []
                    [ text str ]

            Error str ->
                div [] [ text str ]

            Waiting str ->
                div [] [ text str ]
        ]



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    Sub.none

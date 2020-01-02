module Main exposing (main)

import API
import Browser
import Browser.Navigation as Nav
import Data.User as User
import Env exposing (Env)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E
import Page.PostLongWord
import Page.Top
import Page.TypeLongWord
import Page.User
import Page.WordList
import Ports
import Url
import Url.Builder



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { env : Env
    , page : Page
    , postLongWordPageCache : Maybe Page.PostLongWord.Model
    }


type Page
    = NotFound
    | TopPage Page.Top.Model
    | UserPage Page.User.Model
    | PostLongWordPage Page.PostLongWord.Model
    | WordListPage Page.WordList.Model
    | TypeLongWordPage Page.TypeLongWord.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        env =
            { url = url
            , key = key
            , user = User.new
            }

        model =
            { env = env
            , page = NotFound
            , postLongWordPageCache = Nothing
            }

        ( m, c ) =
            goTo url model
    in
    ( m, Cmd.batch [ c, Ports.getUser () ] )



-- UPDATE


type Msg
    = RestoreUser D.Value
    | ReceiveNewUserID (Result Http.Error User.User)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | TopMsg Page.Top.Msg
    | UserMsg Page.User.Msg
    | PostLongWordMsg Page.PostLongWord.Msg
    | WordListMsg Page.WordList.Msg
    | TypeLongWordMsg Page.TypeLongWord.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        pEnv =
            model.env
    in
    case ( msg, model.page ) of
        ( RestoreUser data, _ ) ->
            let
                res =
                    D.decodeValue User.userDecoder data
            in
            case res of
                Ok user ->
                    let
                        newEnv =
                            { pEnv | user = user }

                        newModel =
                            { model | env = newEnv }
                    in
                    goTo newEnv.url newModel

                Err _ ->
                    ( model, API.getNewUserID ReceiveNewUserID )

        ( ReceiveNewUserID (Ok user), _ ) ->
            ( { model | env = { pEnv | user = user } }, Ports.saveUser (User.encodeUser user) )

        ( ReceiveNewUserID (Err e), _ ) ->
            let
                errUser =
                    -- ローカル開発用で
                    User.User "err_id" "err_name"
            in
            ( { model | env = { pEnv | user = errUser } }, Cmd.none )

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl pEnv.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            goTo url model

        ( TopMsg subMsg, TopPage subModel ) ->
            let
                ( newModel, topCmd, newEnv ) =
                    Page.Top.update subMsg subModel model.env
            in
            ( { model | env = newEnv, page = TopPage newModel }
            , Cmd.map TopMsg topCmd
            )

        ( UserMsg subMsg, UserPage subModel ) ->
            let
                ( newModel, topCmd, newEnv ) =
                    Page.User.update subMsg subModel model.env
            in
            ( { model | env = newEnv, page = UserPage newModel }
            , Cmd.map UserMsg topCmd
            )

        ( PostLongWordMsg subMsg, PostLongWordPage subModel ) ->
            let
                ( newModel, topCmd, newEnv ) =
                    Page.PostLongWord.update subMsg subModel model.env
            in
            ( { model | env = newEnv, page = PostLongWordPage newModel, postLongWordPageCache = Just newModel }
            , Cmd.map PostLongWordMsg topCmd
            )

        ( WordListMsg subMsg, WordListPage subModel ) ->
            let
                ( newModel, topCmd, newEnv ) =
                    Page.WordList.update subMsg subModel model.env
            in
            ( { model | env = newEnv, page = WordListPage newModel }
            , Cmd.map WordListMsg topCmd
            )

        ( TypeLongWordMsg subMsg, TypeLongWordPage subModel ) ->
            let
                ( newModel, topCmd, newEnv ) =
                    Page.TypeLongWord.update subMsg subModel model.env
            in
            ( { model | env = newEnv, page = TypeLongWordPage newModel }
            , Cmd.map TypeLongWordMsg topCmd
            )

        ( _, _ ) ->
            ( model, Cmd.none )


goTo : Url.Url -> Model -> ( Model, Cmd Msg )
goTo url model =
    let
        pEnv =
            model.env

        newEnv =
            { pEnv | url = url }

        newModel =
            { model | env = newEnv }
    in
    case newEnv.url.fragment of
        Just "post" ->
            let
                ( pageModel, pageCmd ) =
                    case model.postLongWordPageCache of
                        Just cm ->
                            ( cm, Cmd.none )

                        Nothing ->
                            Page.PostLongWord.init newEnv
            in
            ( { newModel | page = PostLongWordPage pageModel }
            , Cmd.map PostLongWordMsg pageCmd
            )

        Just "user" ->
            let
                ( pageModel, pageCmd ) =
                    Page.User.init newEnv
            in
            ( { newModel | page = UserPage pageModel }
            , Cmd.map UserMsg pageCmd
            )

        Just "list" ->
            let
                ( pageModel, pageCmd ) =
                    Page.WordList.init newEnv
            in
            ( { newModel | page = WordListPage pageModel }
            , Cmd.map WordListMsg pageCmd
            )

        Just "typeLongWord" ->
            let
                ( pageModel, pageCmd ) =
                    Page.TypeLongWord.init newEnv
            in
            ( { newModel | page = TypeLongWordPage pageModel }
            , Cmd.map TypeLongWordMsg pageCmd
            )

        Nothing ->
            let
                ( pageModel, pageCmd ) =
                    Page.TypeLongWord.init newEnv
            in
            ( { newModel | page = TypeLongWordPage pageModel }
            , Cmd.map TypeLongWordMsg pageCmd
            )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        s1 =
            case model.page of
                TypeLongWordPage subModel ->
                    Sub.map TypeLongWordMsg Page.TypeLongWord.subscriptions

                _ ->
                    Sub.none

        s2 =
            Ports.restoreUser RestoreUser
    in
    Sub.batch [ s1, s2 ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "タイピング部(β)"
    , body =
        [ viewHeadder
        , case model.page of
            NotFound ->
                viewNotFound

            TopPage subModel ->
                Page.Top.view subModel
                    |> Html.map TopMsg

            UserPage subModel ->
                Page.User.view subModel
                    |> Html.map UserMsg

            PostLongWordPage subModel ->
                Page.PostLongWord.view subModel
                    |> Html.map PostLongWordMsg

            WordListPage subModel ->
                Page.WordList.view subModel
                    |> Html.map WordListMsg

            TypeLongWordPage subModel ->
                Page.TypeLongWord.view subModel
                    |> Html.map TypeLongWordMsg
        ]
    }


viewHeadder : Html msg
viewHeadder =
    div []
        [ nav [ class "header-nav" ]
            [ ul []
                [ li [] [ a [ href "./" ] [ span [ class "header-nav__top" ] [ text "タイピング部(β)" ] ] ]
                , li [] [ a [ href "./#list" ] [ span [] [ text "お題リスト" ] ] ]
                , li [] [ a [ href "./#post" ] [ span [] [ text "お題投稿" ] ] ]
                , li [] [ a [ href "./#user" ] [ span [] [ text "ユーザー" ] ] ]
                ]
            ]
        ]


viewNotFound : Html msg
viewNotFound =
    div []
        [ text "NotFound"
        ]



-- PROGRAM

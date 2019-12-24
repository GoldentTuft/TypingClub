module Tests exposing (all)

import Expect
import Test exposing (..)
import Typing



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "A Test Suite"
        (List.concat
            [ testSuccess "んっこ"
                [ "xnkko"
                , "xncco"
                , "xnltuko"
                , "xnltuco"
                , "xnxtuko"
                , "xnxtuco"
                , "xnltsuko"
                , "xnltsuco"
                , "xnxtsuko"
                , "xnxtsuco"
                , "nkko"
                , "ncco"
                , "nltuko"
                , "nltuco"
                , "nxtuko"
                , "nxtuco"
                , "nltsuko"
                , "nltsuco"
                , "nxtsuko"
                , "nxtsuco"
                , "nnkko"
                , "nncco"
                , "nnltuko"
                , "nnltuco"
                , "nnxtuko"
                , "nnxtuco"
                , "nnltsuko"
                , "nnltsuco"
                , "nnxtsuko"
                , "nnxtsuco"
                ]
            , testSuccess "んしゃん"
                [ "nsyann"
                , "nnshaxn"
                , "nsilyann"
                ]
            , testFail "んしゃん"
                [ "nsyan"
                , "nnsyan"
                , "nsyank"
                , "nsisyann"
                , "nssyann"
                ]
            , testSuccess "んじゃん"
                [ "njann"
                , "nzyann"
                , "nnzilyann"
                ]
            , testSuccess "かちょう"
                [ "katyou"
                , "cachou"
                , "kachilyowu"
                , "kacyou"
                ]
            , testFail "かちょう"
                [ "kathou"
                ]
            , testSuccess "べーこん"
                [ "be-konn"
                , "be-coxn"
                ]
            , testSuccess "らんにんぐ"
                [ "rannninngu"
                , "raxnningu"
                ]
            , testFail "らんにんぐ"
                [ "ranninngu"
                , "rannninngwo"
                ]
            , testSuccess "んじゃっこん"
                [ "njakkonn"
                , "nnzyaccoxn"
                , "njaltukonn"
                ]
            , testFail "んじゃっこん"
                [ "njakkon"
                , "njaqqonn"
                ]
            , testSuccess "んじゃっ"
                [ "njaltu" ]
            , testFail "んじゃっ"
                [ "njakk"
                , "njak"
                ]
            , testSuccess "んじゃっこ"
                [ "njakko"
                , "nnjacco"
                ]
            , testSuccess "っ"
                [ "ltu"
                , "xtu"
                ]
            , testFail "っ"
                [ "qq"
                , "kko"
                , "q"
                ]
            , testReject "っ"
                [ "q"
                , "qq"
                ]
            , testSuccess "けんで"
                [ "kende"
                , "kennde"
                , "kexnde"
                ]
            , testFail "けんよ"
                [ "kenyo"
                ]
            , testSuccess "けんよ"
                [ "kennyo"
                , "kexnyo"
                ]
            ]
        )


testSuccessHelp : String -> String -> Test
testSuccessHelp words input =
    test ("success " ++ input ++ ":" ++ words) <|
        \_ ->
            case piyo input words of
                Nothing ->
                    Expect.fail (input ++ "(1)")

                Just d ->
                    if d.fixedWords == words then
                        Expect.pass

                    else
                        Expect.fail (input ++ "(2)")


testSuccess : String -> List String -> List Test
testSuccess words inputs =
    List.map (testSuccessHelp words) inputs


testFailHelp : String -> String -> Test
testFailHelp words input =
    test ("fail " ++ input ++ ":" ++ words) <|
        \_ ->
            case piyo input words of
                Nothing ->
                    Expect.pass

                Just d ->
                    if d.fixedWords == words then
                        Expect.fail (input ++ "(1)")

                    else
                        Expect.pass


testFail : String -> List String -> List Test
testFail words inputs =
    List.map (testFailHelp words) inputs


testRejectHelp : String -> String -> Test
testRejectHelp words input =
    test ("reject " ++ input ++ ":" ++ words) <|
        \_ ->
            case piyo input words of
                Nothing ->
                    Expect.pass

                _ ->
                    Expect.fail (input ++ "(1)")


testReject : String -> List String -> List Test
testReject words inputs =
    List.map (testRejectHelp words) inputs


hoge =
    let
        d1 =
            Typing.newData "んしゃ"

        d2 =
            Typing.typeTo "n" d1
                |> Maybe.andThen (Typing.typeTo "s")
                |> Debug.log "2"
                |> Maybe.andThen (Typing.typeTo "s")
                |> Maybe.andThen (Typing.typeTo "n")
                |> Maybe.andThen (Typing.typeTo "n")
    in
    Debug.log "result" d2


piyo : String -> String -> Maybe Typing.Data
piyo input words =
    List.foldl (\k d -> Maybe.andThen (Typing.typeTo k) d) (Just (Typing.newData words)) (String.split "" input)

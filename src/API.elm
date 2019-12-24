module API exposing (..)

import Data.LongWord as LongWord
import Data.User as User
import Http
import Json.Decode as D
import Json.Encode as E


host =
    --"http://duck.digick.jp/typing1"
    "."


api =
    "/TypingClubAPI.php"


getNewUserID : (Result Http.Error User.User -> msg) -> Cmd msg
getNewUserID msg =
    Http.get
        { url = host ++ api ++ "?getNewID"
        , expect = Http.expectJson msg User.userDecoder
        }


reName : (Result Http.Error User.User -> msg) -> User.User -> Cmd msg
reName msg user =
    Http.post
        { url = host ++ api ++ "?modifyUser"
        , body = Http.jsonBody (User.encodeUser user)
        , expect = Http.expectJson msg User.userDecoder
        }


changeUser : (Result Http.Error User.User -> msg) -> User.User -> Cmd msg
changeUser msg user =
    Http.post
        { url = host ++ api ++ "?changeUser"
        , body = Http.jsonBody (User.encodeUser user)
        , expect = Http.expectJson msg User.userDecoder
        }


encodeForPostLongWord : User.User -> LongWord.LongWord -> E.Value
encodeForPostLongWord user longWord =
    E.object
        [ ( "user", User.encodeUser user )
        , ( "longWord", LongWord.encodeLongWord longWord )
        ]


postLongWord : (Result Http.Error String -> msg) -> User.User -> LongWord.LongWord -> Cmd msg
postLongWord msg user lw =
    Http.post
        { url = host ++ api ++ "?postLongWord"
        , body = Http.jsonBody (encodeForPostLongWord user lw)
        , expect = Http.expectString msg
        }


type alias WordSummary =
    { id : Int
    , title : String
    , userName : String
    }


type alias WordList =
    List WordSummary


wordSummaryDecoder : D.Decoder WordSummary
wordSummaryDecoder =
    D.map3 WordSummary
        (D.field "id" D.int)
        (D.field "title" D.string)
        (D.field "userName" D.string)


wordListDecoder : D.Decoder WordList
wordListDecoder =
    D.list wordSummaryDecoder


getAllWordList : (Result Http.Error WordList -> msg) -> Cmd msg
getAllWordList msg =
    Http.get
        { url = host ++ api ++ "?getAllWordList"
        , expect = Http.expectJson msg wordListDecoder
        }


encodeForGetLongWord : Int -> E.Value
encodeForGetLongWord id =
    E.object
        [ ( "id", E.int id )
        ]


getLongWord : (Result Http.Error LongWord.LongWord -> msg) -> Int -> Cmd msg
getLongWord msg id =
    Http.post
        { url = host ++ api ++ "?getLongWord"
        , body = Http.jsonBody (encodeForGetLongWord id)
        , expect = Http.expectJson msg LongWord.longWordDecoder
        }


getPresentLongWord : (Result Http.Error LongWord.LongWord -> msg) -> Cmd msg
getPresentLongWord msg =
    Http.get
        { url = host ++ api ++ "?getPresentLongWord"
        , expect = Http.expectJson msg LongWord.longWordDecoder
        }


encodeForDeleteLongWord : User.User -> Int -> E.Value
encodeForDeleteLongWord user wid =
    E.object
        [ ( "wordID", E.int wid )
        , ( "user", User.encodeUser user )
        ]


deleteLongWord : (Result Http.Error String -> msg) -> User.User -> Int -> Cmd msg
deleteLongWord msg user wid =
    Http.post
        { url = host ++ api ++ "?deleteLongWord"
        , body = Http.jsonBody (encodeForDeleteLongWord user wid)
        , expect = Http.expectString msg
        }


encodeForRegistLongWordRanking : User.User -> Int -> LongWord.Score -> E.Value
encodeForRegistLongWordRanking user wid score =
    E.object
        [ ( "user", User.encodeUser user )
        , ( "wordID", E.int wid )
        , ( "score", LongWord.encodeScore score )
        ]


registLongWordRanking : (Result Http.Error String -> msg) -> User.User -> Int -> LongWord.Score -> Cmd msg
registLongWordRanking msg user wid score =
    Http.post
        { url = host ++ api ++ "?registLongWordRanking"
        , body = Http.jsonBody (encodeForRegistLongWordRanking user wid score)
        , expect = Http.expectString msg
        }


type alias LongWordRankingScore =
    { score : LongWord.Score
    , name : String
    }

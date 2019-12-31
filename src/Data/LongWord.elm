module Data.LongWord exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode as D
import Json.Encode as E


type alias LongWord =
    { title : String
    , id : Int
    , wordForView : String
    , wordForInput : String
    }


type alias Score =
    { time : Int
    , keys : Int
    , miss : Int
    }


new : LongWord
new =
    LongWord "" 0 "" ""


trim : LongWord -> LongWord
trim lw =
    { title = String.trim lw.title
    , id = lw.id
    , wordForView = String.trim lw.wordForView
    , wordForInput = String.trim lw.wordForInput
    }


encodeLongWord : LongWord -> E.Value
encodeLongWord rLongWord =
    let
        longWord =
            trim rLongWord
    in
    E.object
        [ ( "title", E.string longWord.title )
        , ( "wordForView", E.string longWord.wordForView )
        , ( "wordForInput", E.string longWord.wordForInput )
        ]


longWordDecoder : D.Decoder LongWord
longWordDecoder =
    D.map4 LongWord
        (D.field "title" D.string)
        (D.field "id" D.int)
        (D.field "wordForView" D.string)
        (D.field "wordForInput" D.string)


encodeScore : Score -> E.Value
encodeScore v =
    E.object
        [ ( "time", E.int v.time )
        , ( "keys", E.int v.keys )
        , ( "miss", E.int v.miss )
        ]


scoreDecoder : D.Decoder Score
scoreDecoder =
    D.map3 Score
        (D.field "time" D.int)
        (D.field "keys" D.int)
        (D.field "miss" D.int)


validate : LongWord -> Bool
validate rLW =
    let
        lw =
            trim rLW

        lenT =
            String.length lw.title

        lenWV =
            String.length lw.wordForView

        lenWI =
            String.length lw.wordForInput

        c1 =
            0 < lenT && lenT < 150

        c2 =
            0 < lenWV && lenWV < 10000

        c3 =
            0 < lenWI && lenWI < 10000

        c4 =
            lenWV == lenWI
    in
    List.all (\n -> n) [ c1, c2, c3, c4 ]

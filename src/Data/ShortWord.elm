module Data.ShortWord exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode as D
import Json.Encode as E


type alias ShortWords =
    { title : String
    , id : Int
    , words : List Word
    }


type alias Word =
    { wordForView : String
    , wordForInput : String
    }


type alias Score =
    { time : Int
    , keys : Int
    , miss : Int
    }


new : ShortWords
new =
    ShortWords "" 0 []


trimWord : Word -> Word
trimWord w =
    { wordForView = String.trim w.wordForView
    , wordForInput = String.trim w.wordForInput
    }


trim : ShortWords -> ShortWords
trim sw =
    { title = String.trim sw.title
    , id = sw.id
    , words = List.map trimWord sw.words
    }


encodeShortWords : ShortWords -> E.Value
encodeShortWords rShortWords =
    let
        words =
            trim rShortWords
    in
    E.object
        [ ( "title", E.string words.title )
        , ( "words", E.list encodeWord words.words )
        ]


encodeWord : Word -> E.Value
encodeWord w =
    E.object
        [ ( "wordForView", E.string w.wordForView )
        , ( "wordForInput", E.string w.wordForInput )
        ]


wordsDecoder : D.Decoder ShortWords
wordsDecoder =
    D.map3 ShortWords
        (D.field "title" D.string)
        (D.field "id" D.int)
        (D.field "words" (D.list wordDecoder))


wordDecoder : D.Decoder Word
wordDecoder =
    D.map2 Word
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


validateWord : Word -> Bool
validateWord rawWord =
    let
        w =
            trimWord rawWord

        lenWV =
            String.length w.wordForView

        lenWI =
            String.length w.wordForInput

        c1 =
            lenWV == lenWI

        c2 =
            0 < lenWV && lenWV < 60

        c3 =
            0 < lenWI && lenWI < 60
    in
    List.all (\n -> n) [ c1, c2, c3 ]



{- }
   validate : ShortWord -> Bool
   validate rSW =
       let
           sw =
               trim rSW

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
-}

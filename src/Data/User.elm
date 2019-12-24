module Data.User exposing (..)

import Http
import Json.Decode as D
import Json.Encode as E


type alias User =
    { id : String
    , name : String
    }


new : User
new =
    { id = "nothing", name = "nothing" }


trim : User -> User
trim user =
    { id = String.trim user.id, name = String.trim user.name }


encodeUser : User -> E.Value
encodeUser rUser =
    let
        user =
            trim rUser
    in
    E.object
        [ ( "id", E.string user.id )
        , ( "name", E.string user.name )
        ]


userDecoder : D.Decoder User
userDecoder =
    D.map2 User
        (D.field "id" D.string)
        (D.field "name" D.string)

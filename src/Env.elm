module Env exposing (Env)

import Browser.Navigation as Nav
import Data.User as User
import Url


type alias Env =
    { url : Url.Url
    , key : Nav.Key
    , user : User.User
    }

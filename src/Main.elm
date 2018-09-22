module Main exposing (main)

import Browser exposing (document)
import State exposing (init, subscriptions, timedUpdate, validateGuest)
import Types exposing (Model, TimeMsg)
import View exposing (timedView)
import Json.Decode as Decode exposing (Value)

main : Program Value Model TimeMsg
main =
    Browser.document
        { init = init
        , update = timedUpdate
        , view = timedView
        , subscriptions = subscriptions
        }

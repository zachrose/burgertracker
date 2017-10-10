module Main exposing (..)
import Task
import Html
import Time exposing (Time)

import Types exposing (..)
import State exposing (init, timedUpdate, subscriptions, validateGuest)
import View exposing (timedView)

main =
  Html.program
    { init = init
    , update = timedUpdate
    , view = timedView
    , subscriptions = subscriptions
    }

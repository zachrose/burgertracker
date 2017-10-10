module Main exposing (..)

import Html exposing (program)

import State exposing (init, timedUpdate, subscriptions, validateGuest)
import View exposing (timedView)

main =
  program
    { init = init
    , update = timedUpdate
    , view = timedView
    , subscriptions = subscriptions
    }

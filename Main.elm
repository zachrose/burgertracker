module Main exposing (..)

import Html
import Html.Attributes as A
import Html.Events as E

main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }

type alias Model =
  { guests : List Guest
  }

type alias Guest =
  { name : String
  , orders : List GuestOrder
  , time: Time
  }

type alias GuestOrder =
  { guest: Guest
  , time: Time
  , item: MenuItem
  }

type alias RestaurantOrder =
  { items: List GuestOrder
  , time: Time
  }

model : Model
model =
  { guests = [ larry ]
  , knocks = []
  }

type Msg
  = GetTimeAndThen (Time -> Msg)
  | AddGuest String Time
  | TakeOrder GuestOrder Time
  | MakeOrder RestaurantOrder Time

getTime =
  Task.perform OnTime Time.now

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GetTimeAndThen successHandler ->
      ( model, (Task.perform assertNeverHandler successHandler Time.now) )
    AddGuest name time ->
      ( { model | guests = (Guest name [] time) :: model.guests } )

css : String -> Html.Html msg
css path =
  Html.node "link" [ A.rel "stylesheet", A.href path ] []

view : Model -> Html.Html Msg
view model =
  Html.div [] [
    css "style.css"
    -- headerView,
    -- guestsView model.guests,
    -- knocksView model.knocks
  ]






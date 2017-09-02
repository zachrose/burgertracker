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

-- model

type alias MenuItem =
  { name: String
  , priceInCents: Int
  }

hamburger    = MenuItem "Hambrger"      100
cheeseburger = MenuItem "Cheeseburger"  149
fries        = MenuItem "Fries"         125
soda         = MenuItem "Soda"          199

type alias Model =
  { guests : List Guest
  , newGuestName : String
  , newGuestComped : Bool
  , newGuestSubmitted : Bool
  , requests : List Request
  }

type alias Request =
  { guest: Guest
  , item: MenuItem
  }

type alias Guest =
  { name : String
  , comped : Bool
  }

makeGuest : String -> Guest
makeGuest name =
  Guest name False

ben : Guest
ben = makeGuest "ben"

charlie : Guest
charlie = Guest "charlie" True

guests : List Guest
guests = [ ben, charlie ]

requests : List Request
requests = [ Request ben cheeseburger, Request charlie hamburger ]

model : Model
model = Model guests "" False False requests

-- update

type Msg
  = SubmitGuest 
  | AddRequest
  | NewGuestName String
  | NewGuestComped
  | DeleteGuest Guest

without predicate list =
  ( Tuple.second ( List.partition predicate list ) )

update : Msg -> Model -> Model
update msg model =
  case msg of
    SubmitGuest ->
      let
        newModel = { model | newGuestSubmitted = True }
        newGuest = Guest model.newGuestName model.newGuestComped
      in
        if validateGuest newGuest then
          { model |
              guests = newGuest :: model.guests,
              newGuestName = ""
          }
        else
          newModel
    NewGuestName name ->
      { model | newGuestName = name }
    NewGuestComped ->
      { model | newGuestComped = True }
    AddRequest ->
      { model | requests = Request ben cheeseburger :: model.requests }
    DeleteGuest guest ->
      { model | guests = without (\g -> g == guest) model.guests }


-- view

validateGuest : Guest -> Bool
validateGuest guest =
  not ( String.isEmpty guest.name )

guestValidationMessage : Model -> String
guestValidationMessage model =
  let
    guestIsValid = validateGuest (Guest model.newGuestName model.newGuestComped)
    untouched = not model.newGuestSubmitted
  in
    if guestIsValid || untouched then
      ""
    else
      "Guest name cannot be empty"

css : String -> Html.Html msg
css path =
  Html.node "link" [ A.rel "stylesheet", A.href path ] []

viewRequest: Request -> Html.Html Msg
viewRequest request =
  Html.li [] [
    Html.text ( request.guest.name ++ " " ++ request.item.name )
  ]

compedText comped = if comped then " (comped)" else ""

menuItemButton : String -> Html.Html Msg
menuItemButton menuItemName =
  Html.li [E.onClick ( AddRequest )] [ Html.text ("add" ++ menuItemName )]

viewGuest : Guest -> Html.Html Msg
viewGuest guest =
  Html.li []
    [ Html.span []
      [ Html.text guest.name
      , Html.text ( compedText guest.comped )
      ],
      Html.span [ E.onClick ( DeleteGuest guest ) ] [ Html.text " delete" ],
      Html.ul [] (List.map menuItemButton ["hamburger"])
    ]

view : Model -> Html.Html Msg
view model =
  Html.div []
    [ css "style.css"
    , Html.text "bottomless burger bonds"
    , Html.div [ A.id "guests" ]
      [ Html.ul [] (List.map viewGuest model.guests)
      , Html.div []
        [ Html.p [ A.class "error" ] [ Html.text ( guestValidationMessage model ) ]
        , Html.input [ E.onInput NewGuestName, A.type_ "text", A.value model.newGuestName ] []
        , Html.input [ E.onClick NewGuestComped, A.type_ "checkbox" ] []
        , Html.button [ E.onClick SubmitGuest ] [ Html.text "submit" ]
        ]
      ]
    , Html.div [ A.id "requests" ]
      [ Html.ul [] (List.map viewRequest model.requests)
      , Html.div []
        [ Html.button
          [ E.onClick AddRequest ]
          [ Html.text "cheeseburger" ]
        ]
      ]
    ]

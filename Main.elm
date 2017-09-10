module Main exposing (..)

import Html
import Task
import Time
import Html.Attributes as A
import Html.Events as E

import Array


main =
  Html.program
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }

-- model

type Guest
  = NormalGuest String
  | CompedGuest String

type alias MenuItem =
  { name: String
  , priceInCents: Int
  }

type alias Request =
  { guest: Guest
  , item: MenuItem
  }

type OrderStatus
  = Open
  | Ordered
  | Served

orderStatuses = [ Open, Ordered, Served ]

type alias Order =
  { requests: List Request
  , status: OrderStatus
  }

type alias Model =
  { guests : List Guest
  , newGuestComped : Bool
  , newGuestName : String
  , newGuestSubmitted : Bool
  , orders : List Order
  , requests : List Request
  }

-- init

hamburger    = MenuItem "Hambrger"      100
cheeseburger = MenuItem "Cheeseburger"  149
fries        = MenuItem "Fries"         125
soda         = MenuItem "Soda"          199

menuItems = [ hamburger, cheeseburger, fries, soda ]

ben : Guest
ben = NormalGuest "Ben"

charlie : Guest
charlie = CompedGuest "Charlie"

guests : List Guest
guests = [ ben, charlie ]

requests : List Request
requests = [ Request ben cheeseburger, Request charlie hamburger ]

model : Model
model = Model guests False "" False [] requests

init = (model, Cmd.none)

-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- update

type Msg
  = SubmitGuest 
  | AddRequest Guest MenuItem
  | NewOrder
  | AdvanceOrder Order
  | NewGuestName String
  | NewGuestComped
  | DeleteGuest Guest
  | OnTime Time.Time

getTime =
  Task.perform OnTime Time.now

without predicate list =
  ( Tuple.second ( List.partition predicate list ) )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SubmitGuest ->
      let
        newGuestType = if model.newGuestComped then CompedGuest else NormalGuest
        newModel = { model | newGuestSubmitted = True }
        newGuest = newGuestType model.newGuestName
      in
        if validateGuest newGuest then
          ({ model |
              guests = newGuest :: model.guests,
              newGuestName = ""
          }, Cmd.none)
        else
          (newModel, Cmd.none)
    NewOrder ->
      if List.isEmpty model.requests then
        (model, Cmd.none)
      else
        let
          requests = model.requests
          newOrder =
          { requests = requests
          , status = Open
          }
        in
          ({ model |
              orders = newOrder :: model.orders,
              requests = []
          }, Cmd.none )
    AdvanceOrder order ->
      let
        advancedOrder = { order | status = nextOrderAction order.status }
        ordersWithoutOrder = Tuple.second ( List.partition (\o -> o == order) model.orders )
        newOrders = advancedOrder :: ordersWithoutOrder
      in
        ( { model | orders = newOrders }, Cmd.none )
    NewGuestName name ->
      ({ model | newGuestName = name }, Cmd.none )
    NewGuestComped ->
      ({ model | newGuestComped = True }, Cmd.none )
    AddRequest guest menuItem ->
      ({ model | requests = Request guest menuItem :: model.requests }, Cmd.none )
    DeleteGuest guest ->
      ({ model | guests = without (\g -> g == guest) model.guests }, Cmd.none )
    OnTime time ->
      ( model, Cmd.none )


-- view

validateGuest : Guest -> Bool
validateGuest guest =
  True
  -- not ( String.isEmpty guest )

guestValidationMessage : Model -> String
guestValidationMessage model =
  let
    guestIsValid = validateGuest (NormalGuest model.newGuestName)
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
    Html.text ( compedText( request.guest ) ++ " wants a " ++ request.item.name )
  ]

nextOrderAction: OrderStatus -> OrderStatus
nextOrderAction orderStatus =
  case orderStatus of
    Open -> Ordered
    Ordered -> Served
    Served -> Served

orderActionString : OrderStatus -> String
orderActionString orderStatus =
  case orderStatus of
    Open -> "mark as ordered"
    Ordered -> "mark as served"
    Served -> "serve"

viewOrder: Order -> Html.Html Msg
viewOrder order =
  let
    nextActionEl = case order.status of
      Served -> (Html.p [] [Html.text ""])
      _ -> (Html.button [ E.onClick (AdvanceOrder order )] [ Html.text (orderActionString order.status) ])
  in
    Html.li []
    [ Html.ul [] (List.map viewRequest order.requests)
    , nextActionEl
    ]

viewOrderRequest: Request -> Html.Html Msg
viewOrderRequest request =
  Html.li [] [
    Html.text "foo"
  ]

compedText : Guest -> String
compedText guest =
  case guest of
    CompedGuest guest ->
      guest ++ "*"
    NormalGuest guest ->
      guest

menuItemButton : Guest -> MenuItem -> Html.Html Msg
menuItemButton guest menuItem =
  Html.li [] [
    Html.button [E.onClick ( AddRequest guest menuItem)] [ Html.text ("request " ++ menuItem.name )]
  ]

viewGuest : Guest -> Html.Html Msg
viewGuest guest =
  Html.li []
    [ Html.h4 [] [ Html.text (compedText guest )]
    , Html.button [ E.onClick ( DeleteGuest guest ) ] [ Html.text " delete" ]
    , Html.ul [] (List.map (menuItemButton guest) menuItems)
    ]

filterOrderBy : OrderStatus -> List Order -> List Order
filterOrderBy status orders =
  List.filter (\o -> o.status == status) orders

view : Model -> Html.Html Msg
view model =
  Html.div []
    [ css "style.css"
    , Html.header []
      [ Html.h1 [] [ Html.text "Bottomless Burgers" ]
      ]
    , Html.main_ []
      [ Html.section [ A.id "guests" ]
        [ Html.h2 [] [ Html.text "Guests" ]
        , Html.div []
          [ Html.h3 [] [ Html.text "Add Guest" ]
          , Html.p [ A.class "error" ] [ Html.text ( guestValidationMessage model ) ]
          , Html.input [ E.onInput NewGuestName, A.type_ "text", A.value model.newGuestName ] []
          , Html.input [ E.onClick NewGuestComped, A.type_ "checkbox" ] []
          , Html.button [ E.onClick SubmitGuest ] [ Html.text "submit" ]
          ]
        , Html.ul [] (List.map viewGuest model.guests)
        ]
      , Html.section [ A.id "requests" ]
        [ Html.h2 [] [ Html.text "Requests" ]
        , Html.ul [] (List.map viewRequest model.requests)
        , Html.button [ E.onClick NewOrder ] [ Html.text "move to new order" ]
        ]
      , Html.section [ A.id "orders" ]
        [ Html.div [ A.id "open-orders" ]
          [ Html.h2 [] [ Html.text "Open Orders" ]
          , Html.ul [] (List.map viewOrder (filterOrderBy Open model.orders)) ]
        , Html.div [ A.id "ordered-orders" ]
          [ Html.h2 [] [ Html.text "Ordered Orders" ]
          , Html.ul [] (List.map viewOrder (filterOrderBy Ordered model.orders)) ]
        , Html.div [ A.id "served-orders" ]
          [ Html.h2 [] [ Html.text "Served Orders" ]
          , Html.ul [] (List.map viewOrder (filterOrderBy Served model.orders)) ]
        ]
      ]
    ]

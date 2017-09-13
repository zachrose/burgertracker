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

type alias PriceInCents = Int

type alias MenuItem =
  { name: String
  , price: PriceInCents
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
  { menuItems: List MenuItem
  , salesTax: Float
  , guests : List Guest
  , newGuestComped : Bool
  , newGuestName : String
  , newGuestSubmitted : Bool
  , newMenuItemName : String
  , newMenuItemPrice : PriceInCents
  , orders : List Order
  , requests : List Request
  }

-- init

hamburger    = MenuItem "Hamburger"     100
cheeseburger = MenuItem "Cheeseburger"  149
fries        = MenuItem "Fries"         125
soda         = MenuItem "Soda"          199

menuItems = [ hamburger, cheeseburger, fries, soda ]

guests : List Guest
guests = [ ]

requests : List Request
requests = [ ]

taxRate : Float
taxRate = 0.0925

model : Model
model = Model menuItems taxRate guests False "" False "" 0 [] requests

init = (model, Cmd.none)

-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- update

type Msg
  = AddRequest Guest MenuItem
  | CancelRequest Request
  | NewOrder
  | AdvanceOrder Order
  | NewGuestName String
  | NewGuestComped
  | SubmitGuest 
  | NewMenuItemName String
  | NewMenuItemPrice String
  | RemoveMenuItem MenuItem
  | SubmitMenuItem
  | OnTime Time.Time

getTime =
  Task.perform OnTime Time.now

without predicate list =
  ( Tuple.second ( List.partition predicate list ) )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
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
    NewMenuItemName name ->
      ({ model | newMenuItemName = name }, Cmd.none )
    NewMenuItemPrice price ->
      let
        priceInCents = Result.toMaybe (String.toInt price)
      in
      case priceInCents of
        Just priceInCents ->
          ({ model | newMenuItemPrice = priceInCents }, Cmd.none )
        Nothing ->
          ({ model | newMenuItemPrice = 0 }, Cmd.none )
    SubmitMenuItem ->
      let
        newMenuItem = MenuItem model.newMenuItemName model.newMenuItemPrice
      in
        if validateMenuItem newMenuItem then
          ({ model |
            menuItems = newMenuItem :: model.menuItems,
            newMenuItemName = "",
            newMenuItemPrice = 0
          }, Cmd.none)
        else
          ( model , Cmd.none )
    RemoveMenuItem menuItem ->
      let
        isRequest = (\mi -> mi == menuItem)
        partitioned = List.partition isRequest model.menuItems
        matches = Tuple.first partitioned
        others = Tuple.second partitioned
        newMenuItems = List.append (List.drop 1 matches) others
      in
        ({ model | menuItems = newMenuItems }, Cmd.none)
    AddRequest guest menuItem ->
      ({ model | requests = Request guest menuItem :: model.requests }, Cmd.none )
    CancelRequest request ->
      let
        isRequest = (\r -> r == request)
        partitioned = List.partition isRequest model.requests
        matches = Tuple.first partitioned
        others = Tuple.second partitioned
        newRequests = List.append (List.drop 1 matches) others
      in
        ({ model | requests = newRequests }, Cmd.none)
    OnTime time ->
      ( model, Cmd.none )


-- view

validateGuest : Guest -> Bool
validateGuest guest =
  True
  -- not ( String.isEmpty guest )

validateMenuItem: MenuItem -> Bool
validateMenuItem menuItem =
  True

menuItemValidationMessage : Model -> String
menuItemValidationMessage model =
  ""

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

wants : String -> String
wants thing =
  if String.endsWith "ies" thing
    then " wants "
    else " wants a "

viewRequest: Request -> Html.Html Msg
viewRequest request =
  Html.li []
  [ Html.text ( compedText( request.guest ) ++ (wants request.item.name) ++ request.item.name )
  , Html.button [ E.onClick (CancelRequest request) ] [ Html.text "cancel" ]
  ]

viewOrderRequest: Request -> Html.Html Msg
viewOrderRequest request =
  Html.li [] [
    Html.text ( compedText( request.guest ) ++ (wants request.item.name) ++ request.item.name )
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
    [ Html.ul [] (List.map viewOrderRequest order.requests)
    , nextActionEl
    , Html.p [] [ Html.text (format (orderCost order ) ) ]
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

viewMenuItem : MenuItem -> Html.Html Msg
viewMenuItem menuItem =
  Html.li []
  [ Html.text (menuItem.name ++ " ")
  , Html.span [A.class "price"] [ Html.text (format menuItem.price )]
  , Html.button [ E.onClick (RemoveMenuItem menuItem) ] [ Html.text "remove" ]
  ]

viewGuest : List MenuItem -> Guest -> Html.Html Msg
viewGuest menuItems guest =
  Html.li []
    [ Html.h4 [] [ Html.text (compedText guest )]
    , Html.ul [] (List.map (menuItemButton guest) menuItems)
    ]

filterOrderBy : OrderStatus -> List Order -> List Order
filterOrderBy status orders =
  List.filter (\o -> o.status == status) orders

format : PriceInCents -> String
format price =
  let
    dollars = toString (price // 100)
    cents = String.padLeft 2 '0' (toString (rem price 100))
  in
    "$" ++ dollars ++ "." ++ cents

orderCost : Order -> PriceInCents
orderCost order =
  let
    requests = order.requests
    menuItemPrices = List.map (\r -> r.item.price) requests
  in
    List.foldr (+) 0 menuItemPrices

expenses : Model -> PriceInCents
expenses model =
  let
    ordersCosts = List.map (\o -> orderCost o) model.orders
    ordersCost = List.foldr (+) 0 ordersCosts
    requestsCosts = List.map (\r -> r.item.price) model.requests
    requestsCost = List.foldr (+) 0 requestsCosts
  in
    round ( toFloat ( requestsCost + ordersCost ) * ( 1 + model.salesTax ) )

revenue : Model -> PriceInCents
revenue model =
  (List.length model.guests) * 16 * 100

profitable : Model -> Maybe Bool
profitable model =
  if revenue model == 0 && expenses model == 0 then
    Nothing
  else if revenue model > expenses model then
    Just True
  else
    Just False

profitableClass : Model -> String
profitableClass model =
  case profitable model of
    Nothing -> "neither"
    Just True -> "profitable"
    Just False -> "not-profitable"

addGuestView : Model -> Html.Html Msg
addGuestView model =
  Html.div []
    [ Html.h3 [] [ Html.text "Add Guest" ]
    , Html.p [ A.class "error" ] [ Html.text ( guestValidationMessage model ) ]
    , Html.input [ E.onInput NewGuestName, A.type_ "text", A.value model.newGuestName ] []
    , Html.input [ E.onClick NewGuestComped, A.type_ "checkbox" ] []
    , Html.button [ E.onClick SubmitGuest ] [ Html.text "submit" ]
    ]

addMenuItemView : Model -> Html.Html Msg
addMenuItemView model =
  Html.div []
    [ Html.h3 [] [ Html.text "Add Menu Item" ]
    , Html.p [ A.class "error" ] [ Html.text ( menuItemValidationMessage model ) ]
    , Html.input [ E.onInput NewMenuItemName, A.type_ "text", A.value model.newMenuItemName ] []
    , Html.input [ E.onInput NewMenuItemPrice, A.type_ "number", A.value (toString model.newMenuItemPrice ) ] []
    , Html.button [ E.onClick SubmitMenuItem] [ Html.text "submit" ]
    ]

view : Model -> Html.Html Msg
view model =
  Html.body [ A.class (profitableClass model) ]
    [ css "style.css"
    , Html.header []
      [ Html.h1 [] [ Html.text "Bottomless Burgers" ]
      , Html.p [] [ Html.text ("Revenue " ++ (format (revenue model ) ))]
      , Html.p [] [ Html.text ("Expenses " ++ (format (expenses model ) ))]
      ]
    , Html.main_ []
      [ Html.section [ A.id "menu-and-taxes" ]
        [ Html.h2 [] [ Html.text "Menu" ]
        , Html.ul [] (List.map viewMenuItem model.menuItems)
        , addMenuItemView model
        , Html.h2 [] [ Html.text "Tax" ]
        , Html.p [] [ Html.text (toString model.salesTax) ]
        ]
      , Html.section [ A.id "guests" ]
        [ Html.h2 [] [ Html.text "Guests" ]
        , addGuestView model
        , Html.ul [] (List.map (viewGuest model.menuItems )model.guests)
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

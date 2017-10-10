module View exposing (..)

import Types exposing (..)
import State exposing (validateGuest)
import Html
import Html.Attributes as A
import Html.Events as E

import Guest.Types
import MenuItem.Types exposing (MenuItem, PriceInCents)
import Request.Types exposing (Request)
import Guest.View exposing(..)
import MenuItem.View exposing (viewMenuItem, addMenuItemView)
import Request.View exposing (..)

css : String -> Html.Html msg
css path =
  Html.node "link" [ A.rel "stylesheet", A.href path ] []


orderActionString : OrderStatus -> String
orderActionString orderStatus =
  case orderStatus of
    Open -> "mark as ordered"
    Ordered -> "mark as served"
    Served -> "serve"

viewOrder: Types.Order -> Html.Html Msg
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

filterOrderBy : OrderStatus -> List Types.Order -> List Types.Order
filterOrderBy status orders =
  List.filter (\o -> o.status == status) orders

format : PriceInCents -> String
format price =
  let
    dollars = toString (price // 100)
    cents = String.padLeft 2 '0' (toString (rem price 100))
  in
    "$" ++ dollars ++ "." ++ cents

orderCost : Types.Order -> PriceInCents
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


addMemoView: Memo -> Html.Html Msg
addMemoView workingMemo =
  Html.div []
    [ Html.h3 [] [ Html.text "Add Memo" ]
    , Html.input [ E.onInput NewWorkingMemo, A.type_ "text", A.value workingMemo ] []
    , Html.button [ E.onClick SubmitMemo ] [ Html.text "submit" ]
    ]

viewMemo : Memo -> Html.Html Msg
viewMemo memo =
  Html.li []
    [
    Html.text memo
    ]


timedView : Model -> Html.Html TimeMsg
timedView model =
    Html.map GetTimeAndThen (view model)


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
      , Html.section [ A.id "requests-and-memos" ]
        [ Html.h2 [] [ Html.text "Requests" ]
        , Html.ul [] (List.map viewRequest model.requests)
        , Html.button [ E.onClick NewOrder ] [ Html.text "move to new order" ]
        , Html.h2 [] [ Html.text "Memos" ]
        , Html.ul [] ( List.map viewMemo model.memos )
        , addMemoView model.workingMemo
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


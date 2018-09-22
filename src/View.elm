module View exposing (addMemoView, css, expenses, filterOrderBy, format, guestRevenue, orderActionString, orderCost, profitable, profitableClass, requestCost, revenue, timedView, view, viewMemo, viewOrder)

import Browser exposing (Document)
import Guest.Types
import Guest.View exposing (..)
import Html
import Html.Attributes as A
import Html.Events as E
import MenuItem.Types exposing (MenuItem, PriceInCents)
import MenuItem.View exposing (addMenuItemView, adjustTaxView, viewMenuItem)
import Request.Types exposing (Request)
import Request.View exposing (..)
import State exposing (validateGuest)
import Types exposing (..)


css : String -> Html.Html msg
css path =
    Html.node "link" [ A.rel "stylesheet", A.href path ] []


orderActionString : OrderStatus -> String
orderActionString orderStatus =
    case orderStatus of
        Ordered ->
            "mark as served"

        Served ->
            "serve"


viewOrder : Types.Order -> Html.Html Msg
viewOrder order =
    let
        nextActionEl =
            case order.status of
                Served ->
                    Html.p [] [ Html.text "" ]

                _ ->
                    Html.button [ E.onClick (AdvanceOrder order) ] [ Html.text (orderActionString order.status) ]
    in
    Html.li []
        [ Html.ul [] (List.map viewOrderRequest order.requests)
        , nextActionEl
        , Html.p [] [ Html.text (format (orderCost order)) ]
        ]


filterOrderBy : OrderStatus -> List Types.Order -> List Types.Order
filterOrderBy status orders =
    List.filter (\o -> o.status == status) orders


format : PriceInCents -> String
format price =
    let
        dollars =
            String.fromInt (price // 100)

        cents =
            String.padLeft 2 '0' (String.fromInt (remainderBy 100 price))
    in
    "$" ++ dollars ++ "." ++ cents


orderCost : Types.Order -> PriceInCents
orderCost order =
    let
        requests =
            order.requests

        menuItemPrices =
            List.map (\r -> r.item.price) requests
    in
    List.foldr (+) 0 menuItemPrices


requestCost : Request -> PriceInCents
requestCost request =
    request.item.price


expenses : Model -> PriceInCents
expenses model =
    let
        ordersCost =
            List.sum (List.map orderCost model.orders)

        requestsCost =
            List.sum (List.map requestCost model.requests)
    in
    round (toFloat (requestsCost + ordersCost) * (1 + model.salesTax))


guestRevenue : Guest.Types.Guest -> PriceInCents
guestRevenue guest =
    case guest of
        Guest.Types.CompedGuest _ ->
            0

        Guest.Types.NormalGuest _ ->
            16 * 100


revenue : Model -> PriceInCents
revenue model =
    let
        guestsRev =
            List.map guestRevenue model.guests
    in
    List.foldr (+) 0 guestsRev



-- (List.length model.guests) * 16 * 100


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
        Nothing ->
            "neither"

        Just True ->
            "profitable"

        Just False ->
            "not-profitable"


addMemoView : Memo -> Html.Html Msg
addMemoView workingMemo =
    Html.div []
        [ Html.h3 [] [ Html.text "Add Memo" ]
        , Html.input [ E.onInput NewWorkingMemo, A.type_ "text", A.value workingMemo ] []
        , Html.button [ E.onClick SubmitMemo ] [ Html.text "submit" ]
        ]


viewMemo : Memo -> Html.Html Msg
viewMemo memo =
    Html.li []
        [ Html.text memo
        ]


timedView : Model -> Browser.Document TimeMsg
timedView model =
    { title = "foo"
    , body = [ Html.map GetTimeAndThen (view model) ]
    }


view : Model -> Html.Html Msg
view model =
    Html.div [ A.class (profitableClass model) ]
        [ css "style.css"
        , Html.header []
            [ Html.h1 [] [ Html.text "Bottomless Burgers" ]
            , Html.p [] [ Html.text ("Revenue " ++ format (revenue model)) ]
            , Html.p [] [ Html.text ("Expenses " ++ format (expenses model)) ]
            ]
        , Html.main_ []
            [ Html.section [ A.id "menu-and-taxes" ]
                [ Html.h2 [] [ Html.text "Menu" ]
                , Html.ul [] (List.map viewMenuItem model.menuItems)
                , addMenuItemView model
                , Html.h2 [] [ Html.text "Tax" ]
                , Html.p [] [ Html.text (String.fromFloat model.salesTax) ]
                , adjustTaxView model.newSalesTax model.salesTax
                ]
            , Html.section [ A.id "guests" ]
                [ Html.h2 [] [ Html.text "Guests" ]
                , addGuestView model
                , Html.ul [] (List.map (viewGuest model.menuItems) model.guests)
                ]
            , Html.section [ A.id "requests-and-memos" ]
                [ Html.h2 [] [ Html.text "Requests" ]
                , Html.ul [] (List.map viewRequest model.requests)
                , Html.button [ E.onClick NewOrder ] [ Html.text "mark as ordered" ]
                , Html.h2 [] [ Html.text "Memos" ]
                , Html.ul [] (List.map viewMemo model.memos)
                , addMemoView model.workingMemo
                ]
            , Html.section [ A.id "orders" ]
                [ Html.div [ A.id "ordered-orders" ]
                    [ Html.h2 [] [ Html.text "Ordered Orders" ]
                    , Html.ul [] (List.map viewOrder (filterOrderBy Ordered model.orders))
                    ]
                , Html.div [ A.id "served-orders" ]
                    [ Html.h2 [] [ Html.text "Served Orders" ]
                    , Html.ul [] (List.map viewOrder (filterOrderBy Served model.orders))
                    ]
                ]
            ]
        ]

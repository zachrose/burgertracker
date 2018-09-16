module Request.View exposing (viewOrderRequest, viewRequest, wants)

import Guest.View exposing (compedText)
import Html
import Html.Attributes as A
import Html.Events as E
import Request.Types exposing (Request)
import Types exposing (..)


wants : String -> String
wants thing =
    if String.endsWith "ies" thing then
        " wants "

    else
        " wants a "


viewRequest : Request -> Html.Html Msg
viewRequest request =
    Html.li []
        [ Html.text (compedText request.guest ++ wants request.item.name ++ request.item.name)
        , Html.button [ E.onClick (CancelRequest request) ] [ Html.text "cancel" ]
        ]


viewOrderRequest : Request -> Html.Html Msg
viewOrderRequest request =
    Html.li []
        [ Html.text (compedText request.guest ++ wants request.item.name ++ request.item.name)
        ]

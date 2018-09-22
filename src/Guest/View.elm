module Guest.View exposing (addGuestView, compedText, guestValidationMessage, viewGuest)

import Guest.Types
import Html
import Html.Attributes as A
import Html.Events as E
import MenuItem.Types exposing (MenuItem)
import MenuItem.View exposing (menuItemButton)
import State exposing (validateGuest)
import Types exposing (..)


viewGuest : List MenuItem -> Guest.Types.Guest -> Html.Html Msg
viewGuest menuItems guest =
    Html.li []
        [ Html.h4 [] [ Html.text (compedText guest) ]
        , Html.ul [] (List.map (menuItemButton guest) menuItems)
        ]


guestValidationMessage : Model -> String
guestValidationMessage model =
    let
        guestIsValid =
            validateGuest (Guest.Types.NormalGuest model.newGuestName)

        untouched =
            not model.newGuestSubmitted
    in
    if guestIsValid || untouched then
        ""

    else
        "Guest name cannot be empty"


compedText : Guest.Types.Guest -> String
compedText guest =
    case guest of
        Guest.Types.CompedGuest g ->
            g ++ "*"

        Guest.Types.NormalGuest g ->
            g


addGuestView : Model -> Html.Html Msg
addGuestView model =
    Html.div []
        [ Html.h3 [] [ Html.text "Add Guest" ]
        , Html.p [ A.class "error" ] [ Html.text (guestValidationMessage model) ]
        , Html.input [ E.onInput NewGuestName, A.type_ "text", A.value model.newGuestName ] []
        , Html.input [ E.onClick NewGuestComped, A.type_ "checkbox" ] []
        , Html.button [ E.onClick SubmitGuest ] [ Html.text "submit" ]
        ]

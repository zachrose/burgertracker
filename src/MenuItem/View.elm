module MenuItem.View exposing (..)

import Html
import Html.Attributes as A
import Html.Events as E

import Types exposing (..)
import Guest.Types
import MenuItem.Types exposing (MenuItem, PriceInCents)

format : PriceInCents -> String
format price =
  let
    dollars = toString (price // 100)
    cents = String.padLeft 2 '0' (toString (rem price 100))
  in
    "$" ++ dollars ++ "." ++ cents

menuItemButton : Guest.Types.Guest -> MenuItem -> Html.Html Msg
menuItemButton guest menuItem =
  Html.li [] [
    Html.button [E.onClick ( AddRequest guest menuItem)] [ Html.text ("request " ++ menuItem.name )]
  ]

menuItemValidationMessage : Model -> String
menuItemValidationMessage model =
  ""

viewMenuItem : MenuItem -> Html.Html Msg
viewMenuItem menuItem =
  Html.li []
  [ Html.text (menuItem.name ++ " ")
  , Html.span [A.class "price"] [ Html.text (format menuItem.price )]
  , Html.button [ E.onClick (RemoveMenuItem menuItem) ] [ Html.text "remove" ]
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

adjustTaxView : String -> Float -> Html.Html Msg
adjustTaxView newTax tax =
  Html.div []
    [ Html.h3 [] [ Html.text "Adjust Sales Tax" ]
    , Html.p [ ] [ Html.text "e.g. \"0.095\"" ]
    , Html.input [ E.onInput NewTax, A.type_ "number", A.value newTax ] []
    , Html.button [ E.onClick SubmitTax] [ Html.text "submit" ]
    ]

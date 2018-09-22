module MenuItem.Types exposing (MenuItem, PriceInCents)


type alias PriceInCents =
    Int


type alias MenuItem =
    { name : String
    , price : PriceInCents
    }

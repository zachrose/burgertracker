module MenuItem.Types exposing (..)

type alias PriceInCents = Int

type alias MenuItem =
  { name: String
  , price: PriceInCents
  }



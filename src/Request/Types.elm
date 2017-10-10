module Request.Types exposing (..)

import Guest.Types
import MenuItem.Types

type alias Request =
  { guest: Guest.Types.Guest
  , item: MenuItem.Types.MenuItem
  }


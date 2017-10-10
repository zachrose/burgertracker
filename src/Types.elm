module Types exposing (..)
import Time exposing (Time)

import Guest.Types
import MenuItem.Types
import Request.Types


type alias Memo = String

type OrderStatus
  = Open
  | Ordered
  | Served

type alias Order =
  { requests: List Request.Types.Request
  , status: OrderStatus
  }

type alias Model =
  { menuItems: List MenuItem.Types.MenuItem
  , salesTax: Float
  , guests : List Guest.Types.Guest
  , newGuestComped : Bool
  , newGuestName : String
  , newGuestSubmitted : Bool
  , newMenuItemName : String
  , newMenuItemPrice : MenuItem.Types.PriceInCents
  , orders : List Order
  , requests : List Request.Types.Request
  , workingMemo : String
  , memos : List Memo
  }

type Msg
  -- guests
  = NewGuestName String
  | NewGuestComped
  | SubmitGuest
  -- menu items
  | NewMenuItemName String
  | NewMenuItemPrice String
  | RemoveMenuItem MenuItem.Types.MenuItem
  | SubmitMenuItem
  -- requests
  | AddRequest Guest.Types.Guest MenuItem.Types.MenuItem
  | CancelRequest Request.Types.Request
  -- orders
  | NewOrder
  | AdvanceOrder Order
  -- memos
  | NewWorkingMemo String
  | NewMemo Memo
  | SubmitMemo
  | DeleteMemo Memo

type TimeMsg
    = GetTimeAndThen Msg
    | GotTime Msg Time

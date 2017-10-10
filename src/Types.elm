module Types exposing (..)
import Time exposing (Time)

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

type alias Memo = String

type OrderStatus
  = Open
  | Ordered
  | Served

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
  | RemoveMenuItem MenuItem
  | SubmitMenuItem
  -- requests
  | AddRequest Guest MenuItem
  | CancelRequest Request
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

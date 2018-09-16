module State exposing (cheeseburger, fries, guests, hamburger, init, menuItems, initialModel, nextOrderAction, initialRequests, soda, subscriptions, taxRate, timedUpdate, update, validateGuest, validateMenuItem, without)

import Guest.Types
import MenuItem.Types exposing (MenuItem)
import Request.Types exposing (Request)
import Task
import Time exposing (Posix)
import Types exposing (..)



-- init


hamburger =
    MenuItem "Hamburger" 100


cheeseburger =
    MenuItem "Cheeseburger" 149


fries =
    MenuItem "Fries" 125


soda =
    MenuItem "Soda" 199


menuItems =
    [ hamburger, cheeseburger, fries, soda ]


guests : List Guest.Types.Guest
guests =
    []


initialRequests : List Request
initialRequests =
    []


taxRate : Float
taxRate =
    0.0925


initialModel : Model
initialModel =
    Model menuItems (String.fromFloat taxRate) taxRate guests False "" False "" 0 [] initialRequests "" []


init flags =
    ( initialModel, Cmd.none )



-- subscriptions


subscriptions : Model -> Sub TimeMsg
subscriptions model =
    Sub.none



-- update


nextOrderAction : OrderStatus -> OrderStatus
nextOrderAction orderStatus =
    case orderStatus of
        Ordered ->
            Served

        Served ->
            Served


validateGuest : Guest.Types.Guest -> Bool
validateGuest guest =
    True



-- not ( String.isEmpty guest )


validateMenuItem : MenuItem -> Bool
validateMenuItem menuItem =
    True


without predicate list =
    Tuple.second (List.partition predicate list)


timedUpdate : TimeMsg -> Model -> ( Model, Cmd TimeMsg )
timedUpdate msg model =
    case msg of
        GetTimeAndThen wrappedMsg ->
            ( model, Task.perform (GotTime wrappedMsg) Time.now )

        GotTime wrappedMsg time ->
            let
                ( newModel, cmd ) =
                    update wrappedMsg time model
            in
            ( newModel, Cmd.map GetTimeAndThen cmd )


update : Msg -> Posix -> Model -> ( Model, Cmd Msg )
update msg time model =
    case msg of
        NewOrder ->
            if List.isEmpty model.requests then
                ( model, Cmd.none )

            else
                let
                    requests =
                        model.requests

                    newOrder =
                        { requests = requests
                        , status = Ordered
                        }
                in
                ( { model
                    | orders = newOrder :: model.orders
                    , requests = []
                  }
                , Cmd.none
                )

        AdvanceOrder order ->
            let
                advancedOrder =
                    { order | status = nextOrderAction order.status }

                ordersWithoutOrder =
                    Tuple.second (List.partition (\o -> o == order) model.orders)

                newOrders =
                    advancedOrder :: ordersWithoutOrder
            in
            ( { model | orders = newOrders }, Cmd.none )

        NewGuestName name ->
            ( { model | newGuestName = name }, Cmd.none )

        NewGuestComped ->
            ( { model | newGuestComped = True }, Cmd.none )

        SubmitGuest ->
            let
                newGuestType =
                    if model.newGuestComped then
                        Guest.Types.CompedGuest

                    else
                        Guest.Types.NormalGuest

                newModel =
                    { model | newGuestSubmitted = True }

                newGuest =
                    newGuestType model.newGuestName
            in
            if validateGuest newGuest then
                ( { model
                    | guests = newGuest :: model.guests
                    , newGuestName = ""
                  }
                , Cmd.none
                )

            else
                ( newModel, Cmd.none )

        NewMenuItemName name ->
            ( { model | newMenuItemName = name }, Cmd.none )

        NewMenuItemPrice price ->
            let
                priceInCents = String.toInt price
            in
            case priceInCents of
                Nothing ->
                    ( { model | newMenuItemPrice = 0 }, Cmd.none )
                Just int ->
                    ( { model | newMenuItemPrice = int }, Cmd.none )


        SubmitMenuItem ->
            let
                newMenuItem =
                    MenuItem model.newMenuItemName model.newMenuItemPrice
            in
            if validateMenuItem newMenuItem then
                ( { model
                    | menuItems = newMenuItem :: model.menuItems
                    , newMenuItemName = ""
                    , newMenuItemPrice = 0
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        NewTax tax ->
            ( { model | newSalesTax = tax }, Cmd.none )

        SubmitTax ->
            let
                parsedTax =
                    String.toFloat model.newSalesTax
            in
            case parsedTax of
                Just tax ->
                    ( { model | salesTax = tax }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SubmitMemo ->
            let
                newMemo =
                    model.workingMemo
            in
            ( { model
                | memos = newMemo :: model.memos
              }
            , Cmd.none
            )

        RemoveMenuItem menuItem ->
            let
                isRequest =
                    \mi -> mi == menuItem

                partitioned =
                    List.partition isRequest model.menuItems

                matches =
                    Tuple.first partitioned

                others =
                    Tuple.second partitioned

                newMenuItems =
                    List.append (List.drop 1 matches) others
            in
            ( { model | menuItems = newMenuItems }, Cmd.none )

        AddRequest guest menuItem ->
            ( { model | requests = Request guest menuItem :: model.requests }, Cmd.none )

        CancelRequest request ->
            let
                isRequest =
                    \r -> r == request

                partitioned =
                    List.partition isRequest model.requests

                matches =
                    Tuple.first partitioned

                others =
                    Tuple.second partitioned

                newRequests =
                    List.append (List.drop 1 matches) others
            in
            ( { model | requests = newRequests }, Cmd.none )

        NewMemo memo ->
            ( { model | memos = memo :: model.memos }, Cmd.none )

        NewWorkingMemo memo ->
            ( { model | workingMemo = memo }, Cmd.none )

        DeleteMemo memo ->
            let
                isMemo =
                    \m -> m == memo

                partitioned =
                    List.partition isMemo model.memos

                matches =
                    Tuple.first partitioned

                others =
                    Tuple.second partitioned

                newMemos =
                    List.append (List.drop 1 matches) others
            in
            ( { model | memos = newMemos }, Cmd.none )

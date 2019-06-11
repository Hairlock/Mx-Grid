module Common exposing (Battery, BatteryFlowDirection(..), Device, DeviceType(..), ExcessFlow, FlowRate, Grid, HomeStatus(..), Storage, StorageMode(..), TickDirection(..), batteryCapacity, fireAction, formatKWH, formatPrice, pushIf, stringToStorageMode)

import Html exposing (Html, div, h4, i, text)
import Html.Attributes exposing (class)
import Json.Encode as Encode
import Round exposing (round)
import Task exposing (perform, succeed)



-- Types


type alias FlowRate =
    Float


type TickDirection
    = Forward
    | Backward


type alias Battery =
    { flowRate : FlowRate
    , maxFlowRate : FlowRate
    , capacity : Float
    , charge : Float
    }


type StorageMode
    = Save
    | Sell


type HomeStatus
    = HasExcess FlowRate
    | NoExcess


type alias Storage =
    { flowRate : FlowRate
    , capacity : Float
    }


type DeviceType
    = WithStorage Storage
    | OnDemand FlowRate


type alias Device =
    { name : String
    , flowRate : FlowRate
    }


type BatteryFlowDirection
    = Charge
    | Draw


type alias Grid =
    { draw : FlowRate
    , price : Float
    }



-- Html


batteryCapacity : Battery -> Maybe String -> List (Html msg)
batteryCapacity { capacity, charge } title =
    let
        icon =
            if charge > (capacity * 0.75) then
                "full"

            else if charge > (capacity * 0.4) then
                "half"

            else if charge > (capacity * 0.2) then
                "quarter"

            else
                "empty"
    in
    [ h4 [] [ i [ class <| "fas fa-battery-" ++ icon ] [] ]
    , h4 [] [ title |> Maybe.map text |> Maybe.withDefault (text <| formatKWH charge) ]
    ]



-- Utils


pushIf : Bool -> a -> List a -> List a
pushIf pred thing list =
    if pred then
        list ++ [ thing ]

    else
        list


formatKWH : FlowRate -> String
formatKWH rate =
    round 2 rate ++ " kWh"


formatPrice : Float -> String
formatPrice price =
    "£" ++ round 2 price


fireAction : (a -> msg) -> a -> Cmd msg
fireAction msgFn payload =
    perform msgFn (succeed payload)


type alias ExcessFlow =
    { id : Int
    , flow : Float
    , date : String
    }


stringToStorageMode : String -> StorageMode
stringToStorageMode mode =
    case mode of
        "Sell" ->
            Sell

        "Save" ->
            Save

        _ ->
            Save


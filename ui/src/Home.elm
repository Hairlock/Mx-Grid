module Home exposing (Model, init, Msg(..), update, view)

import Array exposing (..)
import Debug exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (Value)
import Models exposing (HomeFlowRates, Tariff(..), homeFlowRatesDecoder, tariffToString)
import RemoteData as RemoteData exposing (..)
import Round exposing (round)



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { battery : Battery
    , currentFlowRate : Maybe HomeFlowRates
    , devices : List Device
    , drawRate : Float
    , grid : Grid
    , homeStatus : HomeStatus
    , indexedHomeFlowRates : Array HomeFlowRates
    , storageMode : StorageMode
    , tick : Int
    , unit : String
    }


init : Array HomeFlowRates -> Model
init rates =
    let
        battery =
            { flowRate = 0
            , maxFlowRate = 0.3
            , capacity = 5
            , charge = 5
            }

        grid =
            { draw = 0.0
            , price = 10
            }
    in
    ( { battery = battery
      , grid = grid
      , drawRate = 0.0
      , devices = []
      , unit = "kWh"
      , storageMode = Save
      , homeStatus = NoExcess
      , tick = 0
      , indexedHomeFlowRates = rates
      , currentFlowRate = Array.get 0 rates
      }
    )


type alias FlowRate =
    Float


type TickDirection
    = Forward
    | Backward


type StorageMode
    = Save
    | Sell


type HomeStatus
    = HasExcess FlowRate
    | NoExcess



-- Device


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



-- Battery


type alias Battery =
    { flowRate : FlowRate
    , maxFlowRate : FlowRate
    , capacity : Float
    , charge : Float
    }


type BatteryFlowDirection
    = Charge
    | Draw



-- Grid


type alias Grid =
    { draw : FlowRate
    , price : Float
    }


type Msg
    = HomeFlowRatesTick TickDirection
    | SelectStorageMode StorageMode



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HomeFlowRatesTick dir ->
            ( tick dir model
            , Cmd.none
            )

        SelectStorageMode mode ->
            ( {model | storageMode = mode }, Cmd.none )


tick : TickDirection -> Model -> Model
tick dir model =
    let
        homeFlowRates =
            model.indexedHomeFlowRates

        nextTick =
            case dir of
                Forward ->
                    model.tick + 1

                Backward ->
                    model.tick - 1

        maybeCurrentFlowRate =
            getcurrentFlowRate nextTick homeFlowRates

        updatedModel =
            maybeCurrentFlowRate
                |> Maybe.withDefault (HomeFlowRates "" 0 Low 0)
                |> (\fr -> updateHome fr model)
    in
    { updatedModel
        | currentFlowRate = maybeCurrentFlowRate
        , tick = nextTick
    }


getcurrentFlowRate : Int -> Array HomeFlowRates -> Maybe HomeFlowRates
getcurrentFlowRate i cs =
    Array.get i cs


updateHome : HomeFlowRates -> Model -> Model
updateHome { demand, solar, tariff } model =
    updateBattery (demand - solar) tariff model


updateBattery :
    FlowRate
    -> Tariff
    -> Model
    -> Model
updateBattery flow tariff model =
    let
        ( battery, storageMode, grid ) =
            ( model.battery
            , model.storageMode
            , model.grid
            )

        { maxFlowRate, capacity } =
            battery

        takeFromBattery take =
            let
                takeMax currentCharge rate =
                    let
                        result =
                            currentCharge - rate
                    in
                    if result > 0 then
                        result

                    else
                        0

                ( charge, flowRate, afterBatteryOutput ) =
                    if take < 0 then
                        ( takeMax battery.charge maxFlowRate, maxFlowRate, 0 - maxFlowRate + take )

                    else if take > maxFlowRate then
                        ( takeMax battery.charge maxFlowRate, maxFlowRate, take - maxFlowRate )

                    else
                        ( takeMax battery.charge take, take, 0 )
            in
            ( { battery
                | charge = charge
                , flowRate = flowRate
              }
            , afterBatteryOutput
            )

        giveToBattery give =
            let
                giveMax currentCharge rate =
                    let
                        result =
                            currentCharge + rate
                    in
                    if result > capacity then
                        capacity

                    else
                        result

                ( charge, flowRate, excess ) =
                    if give < 0 then
                        ( giveMax battery.charge maxFlowRate, maxFlowRate, maxFlowRate + give )

                    else if give > maxFlowRate then
                        -- We're getting more than we need, return the excess
                        ( giveMax battery.charge maxFlowRate, maxFlowRate, give + maxFlowRate )

                    else
                        ( giveMax battery.charge give, give, give + maxFlowRate )
            in
            ( { battery
                | charge = charge
                , flowRate = flowRate
              }
            , excess
            )

        -- Excess Flow of positive means sell to the grid, negative means take from the grid
        ( updatedBattery, excessFlow ) =
            case storageMode of
                Save ->
                    case tariff of
                        Low ->
                            -- Pull from the grid // charge battery and devices
                            giveToBattery flow

                        _ ->
                            takeFromBattery flow

                Sell ->
                    case tariff of
                        High ->
                            -- Pull from battery if needed to keep costs low
                            if flow < 0 then
                                takeFromBattery flow

                            else
                                giveToBattery flow

                        _ ->
                            takeFromBattery flow

        updatedGrid =
            { grid | draw = excessFlow }

        homeStatus =
            if excessFlow > 0 then
                NoExcess

            else
                HasExcess excessFlow
    in
    { model
        | battery = updatedBattery
        , grid = updatedGrid
        , homeStatus = homeStatus
    }


-- VIEW


view : Model -> Html Msg
view model =
    model.currentFlowRate
        |> Maybe.map (\fr -> overviewCard model fr)
        |> Maybe.withDefault
            (div [ class "loading" ]
                [ h1 [] [ text "Loading" ]
                , h1 [] [ i [ class "far fa-hourglass" ] [] ]
                ]
            )


overviewCard : Model -> HomeFlowRates -> Html Msg
overviewCard model currentFlowRate =
    let
        { grid, battery, devices, drawRate } =
            model
    in
    div [ class "overview" ]
        [ div [ class "header" ]
            [ if model.tick == 0 then
                h1 [] [ i [ class "fas fa-times" ] [] ]

              else
                h2 [ onClick <| HomeFlowRatesTick Backward ] [ i [ class "fas fa-chevron-circle-left" ] [] ]
            , h1 [] [ text "Home Usage" ]
            , h2 [ onClick <| HomeFlowRatesTick Forward ] [ i [ class "fas fa-chevron-circle-right" ] [] ]
            ]
        , div [ class "header" ]
            [ div []
                [ h2 [] [ text <| "Tariff: " ++ tariffToString currentFlowRate.tariff ] ]
            , div [ class "storage-container"] 
                [ fieldset [] 
                    [ radio "Save" (model.storageMode == Save) (SelectStorageMode Save)
                    , radio "Sell" (model.storageMode == Sell) (SelectStorageMode Sell) 
                    ]
                ]
            ]
        , div [ class "components" ]
            [ gridCard grid
            , batteryCard battery
            , solarCard currentFlowRate
            , homeCard currentFlowRate
            ]
        ]


type alias ComponentProps =
    { name : String
    , icon : String
    , draw : FlowRate
    }


componentCard : ComponentProps -> Maybe (Html Msg) -> Html Msg
componentCard { name, icon, draw } additionalInfo =
    let
        info =
            additionalInfo
                |> Maybe.withDefault (div [] [])
    in
    div [ class "component" ]
        [ div [ class "icon-wrapper" ]
            [ h4 [] [ i [ class <| "fas fa-" ++ icon ] [] ]
            , h4 [] [ text name ]
            ]
        , div [ class "charging" ]
            [ h4 [] [ text "Draw:" ]
            , h4 [] [ text <| formatKWH draw ]
            ]
        , info
        ]


gridCard : Grid -> Html Msg
gridCard { draw } =
    componentCard { name = "Grid", icon = "plug", draw = draw } Nothing


batteryCard : Battery -> Html Msg
batteryCard { flowRate, charge } =
    componentCard { name = "Battery", icon = "battery-full", draw = flowRate } <|
        Just <|
            div [ class "charging" ]
                [ h4 [] [ text "Charge:" ]
                , h4 [] [ text <| formatKWH charge ]
                ]


homeCard : HomeFlowRates -> Html Msg
homeCard { demand } =
    componentCard { name = "Home", icon = "home", draw = demand } Nothing


solarCard : HomeFlowRates -> Html Msg
solarCard { solar } =
    componentCard { name = "Solar", icon = "solar-panel", draw = solar } Nothing


disconnectedCard : Html Msg
disconnectedCard =
    componentCard { name = "Disconnected", icon = "times", draw = 0 } Nothing


radio : String -> Bool -> Msg -> Html Msg
radio value isChecked msg =
  label
    [ class "radio-label" ]
    [ input [ type_ "radio", name "font-size", onClick msg, checked isChecked ] []
    , text value
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

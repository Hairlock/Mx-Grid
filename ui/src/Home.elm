module Home exposing (ExternalMsg(..), Model, Msg(..), init, update, view)

import Array exposing (..)
import Common exposing (..)
import Debug exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (Value)
import Models exposing (HomeFlowRates, tariffToString)
import RemoteData as RemoteData exposing (..)
import Models exposing (Tariff(..), tariffToPrice)



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { index : Int
    , battery : Battery
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


init : Int -> Array HomeFlowRates -> Model
init index rates =
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
    { index = index
    , battery = battery
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


type Msg
    = BubbleTick TickDirection
    | FireTick TickDirection
    | SelectStorageMode String
    | GoToGrid


type ExternalMsg
    = None
    | TriggerTick TickDirection
    | SetGridView
    | SendExcessFlow ExcessFlow



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg, ExternalMsg )
update msg model =
    case msg of
        BubbleTick dir ->
            ( model
            , Cmd.none
            , TriggerTick dir
            )

        FireTick dir ->
            let
                updated =
                    tick dir model

                extCmd =
                    case updated.homeStatus of
                        HasExcess f ->
                            let
                                date =
                                    model.currentFlowRate
                                        |> Maybe.map .date
                                        |> Maybe.withDefault ""
                            in
                            SendExcessFlow <| ExcessFlow model.index f date

                        _ ->
                            None
            in
            ( updated
            , Cmd.none
            , extCmd
            )

        SelectStorageMode mode ->
            ( { model | storageMode = stringToStorageMode mode }, Cmd.none, None )

        GoToGrid ->
            ( model, Cmd.none, SetGridView )


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
            if battery.charge < 0.1 then
                giveToBattery flow

            else
                case storageMode of
                    Save ->
                        case tariff of
                            High ->
                                -- Pull from the grid // charge battery and devices
                                takeFromBattery flow

                            _ ->
                                giveToBattery flow

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
            if excessFlow < 0 then
                HasExcess excessFlow

            else
                NoExcess
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
                h2 [ onClick <| BubbleTick Backward ] [ i [ class "fas fa-chevron-circle-left" ] [] ]
            , h1 [] [ text "Home Usage" ]
            , h2 [ onClick <| BubbleTick Forward ] [ i [ class "fas fa-chevron-circle-right" ] [] ]
            ]
        , div [ class "header" ]
            [ div []
                [ h2 [] [ text <| "Tariff: " ++ tariffToString currentFlowRate.tariff ] ]
            , div [ class "storage-container" ]
                [ select [ onInput SelectStorageMode ]
                    [ option [ value "Sell", selected (model.storageMode == Sell) ] [ text "Sell" ]
                    , option [ value "Save", selected (model.storageMode == Save) ] [ text "Save" ]
                    ]
                ]
            , div [ onClick GoToGrid ]
                [ button [] [ i [ class "fas fa-border-all" ] [], text " Back to Grid" ] ]
            ]
        , div [ class "components" ]
            [ gridCard grid currentFlowRate.tariff
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


gridCard : Grid -> Tariff -> Html Msg
gridCard { draw, price } tariff =
    let
        tariffPrice =
            tariffToPrice tariff

        profit =
            tariffPrice * draw
    in
    componentCard { name = "Grid", icon = "plug", draw = draw } <|
        Just <|
            div [ class "charging" ]
                (if draw < 0 then
                    [ h4 [] [ text <| "Tariff:" ++ formatPrice tariffPrice ]
                    , h4 [] [ text <| "Profit:" ++ formatPrice (0 - profit) ]
                    ]

                 else
                    []
                )


batteryCard : Battery -> Html Msg
batteryCard battery =
    div [ class "component" ]
        [ div [ class "icon-wrapper" ]
            (batteryCapacity battery (Just "Battery"))
        , div [ class "charging" ]
            [ h4 [] [ text "Draw:" ]
            , h4 [] [ text <| formatKWH battery.flowRate ]
            ]
        , div [ class "charging" ]
            [ h4 [] [ text "Charge:" ]
            , h4 [] [ text <| formatKWH battery.charge ]
            ]
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

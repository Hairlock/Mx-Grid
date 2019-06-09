module Main exposing (main)

import Array exposing (..)
import Browser
import Debug exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (Value)
import Models exposing (HomeFlowRates, Tariff(..), homeFlowRatesDecoder, tariffToString)
import RemoteData as RemoteData exposing (..)
import Round exposing (round)

import Home as Home



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { tick : Int
    , flowRateData : WebData (List HomeFlowRates)
    , indexedHomeFlowRates : Array HomeFlowRates
    , homes : Array Home.Model
    }


init : () -> ( Model, Cmd Msg )
init _ =
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

        home =
            Home.init
    in
    ( { tick = 0
      , flowRateData = NotAsked
      , indexedHomeFlowRates = Array.empty
      , homes = Array.empty
      }
    , fetchConsumption
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
    = HomeFlowRatesLoaded (WebData (List HomeFlowRates))
    | HomeFlowRatesTick TickDirection
    | HomeMsg Home.Msg



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HomeFlowRatesLoaded data ->
            let
                rates =
                    case data of
                        Success homeRates ->
                            let
                                indexedRates =
                                    Array.fromList homeRates
                            in                            
                            indexedRates
                        
                        _ ->
                            Array.empty

                updatedModel =
                    { model
                        | flowRateData = data
                        , indexedHomeFlowRates = rates
                        , homes = Array.push (Home.init rates) model.homes
                    }
            in
            ( updatedModel
            , Cmd.none
            )

        HomeFlowRatesTick dir ->
            ( model
            , Cmd.none
            )

        HomeMsg subMsg ->
            let
                (subModel, extMsg) =
                    model.homes
                        |> Array.get 0 
                        |> Maybe.map (\h -> Home.update subMsg h)
                        |> Maybe.withDefault (Home.init model.indexedHomeFlowRates, Cmd.none)
            in
            ({model | homes = Array.set 0 subModel model.homes}, Cmd.none)
            




-- API


apiUrl : String -> String
apiUrl endpoint =
    "http://localhost:3000/" ++ endpoint


fetchConsumption : Cmd Msg
fetchConsumption =
    Http.request
        { method = "GET"
        , headers = []
        , url =
            apiUrl "consumption"
        , body = Http.emptyBody
        , expect = Http.expectJson (RemoteData.fromResult >> HomeFlowRatesLoaded) (Decode.list homeFlowRatesDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }



-- VIEW


view : Model -> Html Msg
view model =
    model.homes
        |> Array.get 0
        |> Maybe.map 
            (\h ->
                Home.view h
                    |> Html.map HomeMsg
            )
        |> Maybe.withDefault 
            (div [ class "loading" ]
                [ h1 [] [ text "Loading" ]
                , h1 [] [ i [ class "far fa-hourglass" ] [] ]
                ]
            )
    -- model.currentFlowRate
    --     |> Maybe.map (\fr -> overviewCard model fr)
    --     |> Maybe.withDefault
            -- (div [ class "loading" ]
            --     [ h1 [] [ text "Loading" ]
            --     , h1 [] [ i [ class "far fa-hourglass" ] [] ]
            --     ]
            -- )

-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none

        -- , subscriptions = \_ -> Sub.none
        -- , onUrlRequest = ClickedLink
        -- , onUrlChange = ChangedUrl
        }



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

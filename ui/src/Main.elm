module Main exposing (main)

import Array exposing (..)
import Browser
import Common exposing (..)
import Debug exposing (..)
import Home as Home
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (Value)
import List.Extra as List
import Models exposing (HomeFlowRates, excessFlowDecoder, excessFlowEncoder, homeFlowRatesDecoder, tariffToPrice, tariffToString)
import RemoteData as RemoteData exposing (..)



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { tick : Int
    , flowRateData : WebData (List HomeFlowRates)
    , homes : List Home.Model
    , storageMode : StorageMode
    , appView : AppView
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tick = 0
      , flowRateData = NotAsked
      , homes = []
      , storageMode = Save
      , appView = Grid
      }
    , fetchHomeRates
    )


type Msg
    = HomeFlowRatesLoaded (WebData (List HomeFlowRates))
    | GridTick TickDirection
    | HomeView Int
    | GridView
    | HomeMsg Int Home.Msg
    | ExcessFlowSent Int (Result Http.Error ExcessFlow)
    | SetStorageMode String
    | Reset


type AppView
    = Grid
    | Home Int



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HomeFlowRatesLoaded data ->
            let
                homes =
                    case data of
                        Success homeRates ->
                            homeRates
                                |> List.groupsOf 100
                                |> List.indexedMap (\i hr -> Home.init i (Array.fromList hr))

                        _ ->
                            []

                updatedModel =
                    { model
                        | flowRateData = data
                        , homes = homes
                    }
            in
            ( updatedModel
            , Cmd.none
            )

        GridTick dir ->
            let
                stuff =
                    model.homes
                        |> List.indexedMap
                            (\i h ->
                                let
                                    ( m, _, ext ) =
                                        Home.update (Home.FireTick dir) h

                                    cmd =
                                        case ext of
                                            Home.SendExcessFlow flow ->
                                                sendExcessFlow i flow

                                            _ ->
                                                Cmd.none
                                in
                                ( m, cmd )
                            )

                ( homes, cmds ) =
                    ( List.map Tuple.first stuff
                    , List.map Tuple.second stuff
                    )
            in
            ( { model
                | homes = homes
                , tick =
                    if dir == Forward then
                        model.tick + 1

                    else
                        model.tick - 1
              }
            , Cmd.batch cmds
            )

        HomeView i ->
            ( { model
                | appView = Home i
              }
            , Cmd.none
            )

        GridView ->
            ( { model
                | appView = Grid
              }
            , Cmd.none
            )

        HomeMsg i subMsg ->
            model.homes
                |> List.getAt i
                |> Maybe.map (Home.update subMsg)
                |> Maybe.map
                    (\( h, subCmd, extMsg ) ->
                        let
                            extCmd =
                                case extMsg of
                                    Home.TriggerTick dir ->
                                        fireAction GridTick dir

                                    Home.SetGridView ->
                                        fireAction (\_ -> GridView) ()

                                    _ ->
                                        Cmd.none
                        in
                        ( { model
                            | homes =
                                model.homes
                                    |> List.updateAt i (\_ -> h)
                          }
                        , Cmd.batch
                            [ Cmd.map (HomeMsg i) subCmd
                            , extCmd
                            ]
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        ExcessFlowSent i (Ok flow) ->
            ( model, Cmd.none )

        ExcessFlowSent _ _ ->
            ( model, Cmd.none )

        Reset ->
            ( model, fireAction HomeFlowRatesLoaded model.flowRateData )

        SetStorageMode m ->
            let
                updatedHomes =
                    model.homes
                        |> List.map
                            (\h ->
                                let
                                    ( updated, _, _ ) =
                                        Home.update (Home.SelectStorageMode m) h
                                in
                                updated
                            )
            in
            ( { model
                | homes = updatedHomes
                , storageMode = stringToStorageMode m
              }
            , Cmd.none
            )



-- API


apiUrl : String -> String
apiUrl endpoint =
    "http://localhost:3000/" ++ endpoint


fetchHomeRates : Cmd Msg
fetchHomeRates =
    Http.request
        { method = "GET"
        , headers = []
        , url =
            apiUrl "homerates?page=0&limit=1000"
        , body = Http.emptyBody
        , expect = Http.expectJson (RemoteData.fromResult >> HomeFlowRatesLoaded) (Decode.list homeFlowRatesDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


sendExcessFlow : Int -> ExcessFlow -> Cmd Msg
sendExcessFlow ix flow =
    Http.request
        { method = "POST"
        , headers = []
        , url =
            apiUrl "grid"
        , body = Http.jsonBody (excessFlowEncoder flow)
        , expect = Http.expectJson (ExcessFlowSent ix) excessFlowDecoder
        , timeout = Nothing
        , tracker = Nothing
        }



-- VIEW


view : Model -> Html Msg
view model =
    case ( List.isEmpty model.homes, model.appView ) of
        ( True, _ ) ->
            div [ class "loading" ]
                [ h1 [] [ text "Loading" ]
                , h1 [] [ i [ class "far fa-hourglass" ] [] ]
                ]

        ( _, Grid ) ->
            div [ class "homes" ]
                [ div [ class "header" ]
                    [ if model.tick == 0 then
                        h1 [] [ i [ class "fas fa-times" ] [] ]

                      else
                        h2 [ onClick <| GridTick Backward ]
                            [ i [ class "fas fa-chevron-circle-left" ] []
                            ]
                    , h1 [] [ text "Grid Overview" ]
                    , h2 [ onClick <| GridTick Forward ] [ i [ class "fas fa-chevron-circle-right" ] [] ]
                    ]
                , div [ class "header" ]
                    [ button [ onClick Reset ] [ text "Reset" ]
                    , div [ class "storage-container" ]
                        [ select [ onInput SetStorageMode ]
                            [ option [ value "Sell", selected (model.storageMode == Sell) ] [ text "Sell" ]
                            , option [ value "Save", selected (model.storageMode == Save) ] [ text "Save" ]
                            ]
                        ]
                    ]
                , div [ class "home-grid" ]
                    (model.homes
                        |> List.indexedMap
                            (\ix { battery, homeStatus, currentFlowRate } ->
                                div [ class "home-card", onClick <| HomeView ix ]
                                    [ div [ class "icon-wrapper" ]
                                        [ h4 [] [ i [ class "fas fa-home" ] [] ]
                                        , h4 [] [ text <| "Home #" ++ (String.fromInt <| ix + 1) ]
                                        ]
                                    , div [ class "icon-wrapper" ]
                                        (batteryCapacity battery)
                                    , div [ class "icon-wrapper" ]
                                        (excessCapacity homeStatus)
                                    , tariffCalculator currentFlowRate homeStatus
                                    ]
                            )
                    )
                ]

        ( _, Home i ) ->
            model.homes
                |> List.getAt i
                |> Maybe.map
                    (\h ->
                        Home.view h
                            |> Html.map (HomeMsg i)
                    )
                |> Maybe.withDefault
                    (div [] [ text "Error loading home" ])


tariffCalculator : Maybe HomeFlowRates -> HomeStatus -> Html Msg
tariffCalculator maybeRates status =
    case ( maybeRates, status ) of
        ( Just { tariff }, HasExcess excess ) ->
            let
                tariffPrice =
                    tariffToPrice tariff

                profit =
                    tariffPrice * excess
            in
            div [ class "icon-wrapper" ]
                [ h4 [] [ i [ class "fas fa-pound-sign --pull-left" ] [] ]
                , h4 [] [ text <| formatPrice (0 - profit) ++ "  " ]
                ]

        ( _, _ ) ->
            div [] []


batteryCapacity : Battery -> List (Html Msg)
batteryCapacity { capacity, charge } =
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
    , h4 [] [ text <| formatKWH charge ]
    ]


excessCapacity : HomeStatus -> List (Html Msg)
excessCapacity homeStatus =
    case homeStatus of
        HasExcess amnt ->
            [ h4 [] [ i [ class "fas fa-check" ] [] ]
            , h4 [] [ text <| formatKWH amnt ]
            ]

        _ ->
            [ h4 [] [ i [ class "fas fa-times" ] [] ]
            , h4 [] [ text "No Excess" ]
            ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }

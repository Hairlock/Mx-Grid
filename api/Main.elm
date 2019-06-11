port module Api.Main exposing (main)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Models exposing (HomeFlowRates, homeFlowRatesDecoder, homeFlowRatesEncoder)
import Serverless
import Serverless.Conn as Conn exposing (jsonBody, interop, respond, route, textBody)
import Serverless.Conn.Request as Request
import UrlParser exposing ((</>), s, map, oneOf, top)
import Dict as Dict
import Result as Result

main : Serverless.Program () () Route Interop Msg
main =
    Serverless.httpApi
        { configDecoder = Serverless.noConfig
        , initialModel = ()
        , parseRoute =
            UrlParser.parseString <|
                oneOf
                    [ map GetHomeFlowRates top ]
        , update = update
        , interop = Serverless.Interop encodeInterop interopDecoder
        , endpoint = endpoint
        , requestPort = requestPort
        , responsePort = responsePort
        }



-- Interop


type Interop
    = FetchHomeFlowRates Int Int


encodeInterop : Interop -> Encode.Value
encodeInterop interop  =
    case interop of 
        FetchHomeFlowRates page limit ->
            Encode.object 
                [ ("page", Encode.int page )
                , ("limit", Encode.int limit ) 
                ]


interopDecoder : String -> Maybe (Decoder Msg)
interopDecoder name =
    case name of
        "fetchHomeFlowRates" ->
            Just <| Decode.map HomeFlowRatesRequest (Decode.list homeFlowRatesDecoder)

        _ ->
            Nothing

-- ROUTES

type Route
    = Home
    | GetHomeFlowRates


endpoint : Conn -> ( Conn, Cmd Msg )
endpoint conn =
    case route conn of
        Home ->
            respond ( 200, textBody "Moixa api" ) conn

        GetHomeFlowRates ->
            let
                (page, limit) =
                    conn
                        |> Conn.request
                        |> (\r ->
                            let
                                queryToInt query stringDefault intDefault =
                                    r
                                        |> Request.query query
                                        |> Maybe.withDefault stringDefault
                                        |> String.toInt
                                        |> Result.toMaybe 
                                        |> Maybe.withDefault intDefault
                            in
                            ( queryToInt "page" "0" 0
                            , queryToInt "limit" "499" 499
                            )
                        )
            in
            interop [ FetchHomeFlowRates page limit ] conn    


type Msg
    = HomeFlowRatesRequest (List HomeFlowRates)


update : Msg -> Conn -> ( Conn, Cmd Msg )
update msg conn =
    case msg of
        HomeFlowRatesRequest c ->
            let
                homeFlowRates =
                    List.map homeFlowRatesEncoder c
                        |> Encode.list
            in
            respond ( 200, jsonBody homeFlowRates ) conn

type alias Conn =
    Conn.Conn () () Route Interop


port requestPort : Serverless.RequestPort msg


port responsePort : Serverless.ResponsePort msg

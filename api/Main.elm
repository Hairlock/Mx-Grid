port module Api.Main exposing (main)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Models exposing (HomeFlowRates, homeFlowRatesDecoder, homeFlowRatesEncoder)
import Serverless
import Serverless.Conn exposing (jsonBody, interop, respond, route, textBody)
import UrlParser exposing ((</>), s, map, oneOf, top)


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
    = FetchHomeFlowRates


encodeInterop : Interop -> Encode.Value
encodeInterop _  =
    Encode.object []


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
            interop [ FetchHomeFlowRates ] conn    


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
    Serverless.Conn.Conn () () Route Interop


port requestPort : Serverless.RequestPort msg


port responsePort : Serverless.ResponsePort msg

port module Grid.Api exposing (Conn, Interop(..), Msg(..), Route(..), encodeInterop, endpoint, interopDecoder, main, requestPort, responsePort, update)

import Json.Decode as Decode exposing (Decoder, decodeValue)
import Json.Encode as Encode
import Serverless
import Serverless.Conn exposing (method, request, respond, jsonBody, textBody, interop, route, Conn(..))
import Serverless.Conn.Request exposing (Method(..), asJson, body)
import UrlParser exposing ((</>), int, map, oneOf, s, top)
import Models exposing (ExcessFlow, excessFlowDecoder, excessFlowEncoder)



{-| Shows how to call JavaScript functions.
-}
main : Serverless.Program () () Route Interop Msg
main =
    Serverless.httpApi
        { configDecoder = Serverless.noConfig
        , initialModel = ()
        , requestPort = requestPort
        , responsePort = responsePort

        , parseRoute =
            UrlParser.parseString <|
                oneOf
                    [ map AddExcessFlow (s "addflow") 
                    , map FetchExcessFlows top
                    ]

        , endpoint = endpoint

        , interop = Serverless.Interop encodeInterop interopDecoder

        , update = update
        }



-- INTEROP


type Interop
    = GetFlows
    | AddFlow ExcessFlow


encodeInterop : Interop -> Encode.Value
encodeInterop interop =
    case interop of
        GetFlows ->
            Encode.object []

        AddFlow f ->
            excessFlowEncoder f


interopDecoder : String -> Maybe (Decoder Msg)
interopDecoder name =
    case name of
        "getFlows" ->
            Just <| Decode.map SendExcessFlow (Decode.list excessFlowDecoder)

        "addFlow" ->
            Just <| Decode.map ExcessFlowAdded excessFlowDecoder

        _ ->
            Nothing



-- ROUTING


type Route
    = FetchExcessFlows
    | AddExcessFlow


endpoint : Conn -> ( Conn, Cmd Msg )
endpoint conn =
    case method conn of
        POST ->
            let
                flow =
                    conn
                        |> (request >> body >> asJson)
                        |> Result.andThen (decodeValue excessFlowDecoder)
                        |> Result.withDefault (ExcessFlow 0 0 "")
            in
            interop [ AddFlow flow ] conn

        GET ->
            interop [ GetFlows ] conn

        _ ->
            respond (404, textBody "Path not found") conn


-- UPDATE

type alias ExcessRate =
    { flow : Float
    , date : String
    }

type Msg
    = SendExcessFlow (List ExcessFlow)
    | ExcessFlowAdded ExcessFlow


update : Msg -> Conn -> ( Conn, Cmd Msg )
update msg conn =
    case msg of
        SendExcessFlow x ->
            let
                excessFlowRates =
                    List.map excessFlowEncoder x
                        |> Encode.list
            in
            respond ( 200, jsonBody excessFlowRates ) conn 

        ExcessFlowAdded f ->
            respond ( 200, jsonBody (excessFlowEncoder f)) conn




-- TYPES


type alias Conn =
    Serverless.Conn.Conn () () Route Interop


port requestPort : Serverless.RequestPort msg


port responsePort : Serverless.ResponsePort msg

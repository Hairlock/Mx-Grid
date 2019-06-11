module Models exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode



type alias HomeFlowRates =
    { date : String
    , demand : Float
    , tariff : Tariff
    , solar : Float
    }     


homeFlowRatesEncoder : HomeFlowRates -> Encode.Value
homeFlowRatesEncoder fr =
    Encode.object
        [ ( "date", Encode.string fr.date )
        , ( "demand", Encode.float fr.demand )
        , ( "tariff", tariffEncoder fr.tariff )
        , ( "solar", Encode.float fr.solar)
        ]


homeFlowRatesDecoder : Decoder HomeFlowRates
homeFlowRatesDecoder =
    Decode.succeed HomeFlowRates
        |> required "date" Decode.string
        |> required "demand" Decode.float
        |> required "tariff" (Decode.string 
                                |> Decode.andThen (\t -> 
                                                    case t of
                                                        "Low" ->
                                                            Decode.succeed Low
                                                        "Normal" ->
                                                            Decode.succeed Normal
                                                        "High" ->
                                                            Decode.succeed High

                                                        _ ->
                                                            Decode.fail "Unknown Tariff"
                                                    )  
                            )
        |> required "solar" Decode.float


tariffToString : Tariff -> String
tariffToString t =
    case t of
        High ->
            "High"

        Normal ->
            "Normal"

        Low ->
            "Low"



type alias ExcessFlow = 
    { id : Int
    , flow : Float
    , date : String
    }
    

excessFlowDecoder : Decoder ExcessFlow
excessFlowDecoder =
    Decode.succeed ExcessFlow
        |> required "id" Decode.int
        |> required "flow" Decode.float
        |> required "date" Decode.string


excessFlowEncoder : ExcessFlow -> Encode.Value
excessFlowEncoder { id, flow, date} =
    Encode.object
        [ ("id", Encode.int id)
        , ("flow", Encode.float flow)
        , ("date", Encode.string date)
        ]        


type Tariff
    = Low
    | Normal
    | High


tariffEncoder : Tariff -> Encode.Value
tariffEncoder tariff =
    case tariff of
        Low ->
            Encode.string "Low"

        Normal ->
            Encode.string "Normal"

        High ->
            Encode.string "High"


tariffToPrice : Tariff -> Float
tariffToPrice tariff =
    case tariff of
        High ->
            0.67

        Normal ->
            0.12

        Low ->
            0.04
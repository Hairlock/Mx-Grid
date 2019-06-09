module Models exposing (HomeFlowRates, homeFlowRatesEncoder, homeFlowRatesDecoder, Tariff(..), tariffToString)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode

type alias HomeFlowRates =
    { date : String
    , demand : Float
    , tariff : Tariff
    , solar : Float
    }    

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


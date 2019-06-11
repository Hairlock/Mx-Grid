Module Decoders exposing (all)

import Json.Encode as Encode
import Test exposing (describe)
import Test.Extra exposing (DecoderExpectation(..), describeDecoder)
import Models

all : Test.Test
all =
    describe "Moixa Api Decoder Tests"
        [ describeDecoder "Models.excessFlowDecoder"
            Models.excessFlowDecoder
            [ ( "null", FailsToDecode )
            , ("{\"id\": 1, \"flow\": 0.5, \"date\": \"24\/05\/90\"}"
            , DecodesTo <| Models.ExcessFlow 1 0.5 "24/05/90")
            ]
        ]


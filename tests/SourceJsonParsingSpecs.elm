module SourceJsonParsingSpecs exposing (suite)

import Data.Post exposing (Source(..))
import Expect
import Json.Decode as Decoder exposing (Error(..), Value)
import Query.Json.SourceDecoder exposing (decodeSource)
import Test exposing (Test, describe, test)



suite : Test
suite =
    describe "Post Json decoders"
        [ describe "should decode correctly"
            [
              test "'MySelf' as a Source" <|
                \_ ->
                    """{ "MySelf": {} }"""
                        |> Decoder.decodeString decodeSource
                        |> Expect.equal (Ok MySelf)
            ]
        ]
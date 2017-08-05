module Tests exposing (..)

{-| All tests copie and adapted from Elm core.
-}

import EverySet exposing (EverySet)
import Expect
import Test exposing (..)


set : EverySet Int
set =
    EverySet.fromList <| List.range 1 100


setPart1 : EverySet Int
setPart1 =
    EverySet.fromList <| List.range 1 50


setPart2 : EverySet Int
setPart2 =
    EverySet.fromList <| List.range 51 100


pred : Int -> Bool
pred x =
    x <= 50


tests : Test
tests =
    let
        queryTests =
            describe "query Tests"
                [ test "size of set of 100 elements" <|
                    \() -> Expect.equal 100 (EverySet.size set)
                ]

        filterTests =
            describe "filter Tests"
                [ test "Simple filter" <|
                    \() -> Expect.equal setPart1 <| EverySet.filter pred set
                ]

        partitionTests =
            describe "partition Tests"
                [ test "Simple partition" <|
                    \() -> Expect.equal ( setPart1, setPart2 ) <| EverySet.partition pred set
                ]
    in
    describe "Set Tests" [ queryTests, partitionTests, filterTests ]

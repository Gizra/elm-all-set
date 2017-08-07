module Tests exposing (..)

{-| All tests copie and adapted from Elm core.
-}

import EverySet exposing (EverySet)
import Expect
import Test exposing (..)


type MyInt
    = MyInt Int


set : EverySet MyInt
set =
    EverySet.fromList <| List.map MyInt <| List.range 1 100


setPart1 : EverySet MyInt
setPart1 =
    EverySet.fromList <| List.map MyInt <| List.range 1 50


setPart2 : EverySet MyInt
setPart2 =
    EverySet.fromList <| List.map MyInt <| List.range 51 100


pred : MyInt -> Bool
pred (MyInt x) =
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

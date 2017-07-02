module Main exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import MultiInput
import Regex
import Test exposing (..)


suite : Test
suite =
    describe "MultiInput"
        [ describe "update"
            [ test "a backspace removes the last item and goes into edit mode" <|
                \_ ->
                    let
                        ( state, items, _ ) =
                            MultiInput.update updateConfig (MultiInput.KeyDown 8) { defaultInitState | nextItem = "" } [ "first", "previous" ]
                    in
                    Expect.equal ( "previous", [ "first" ] ) ( state.nextItem, items )
            , test "a backspace does nothing special when there are no items" <|
                \_ ->
                    let
                        ( state, items, _ ) =
                            MultiInput.update updateConfig (MultiInput.KeyDown 8) { defaultInitState | nextItem = "" } []
                    in
                    Expect.equal ( "", [] ) ( state.nextItem, items )
            , test "a backspace does nothing special when there is a current item" <|
                \_ ->
                    let
                        ( state, items, _ ) =
                            MultiInput.update updateConfig (MultiInput.KeyDown 8) { defaultInitState | nextItem = "something" } [ "other" ]
                    in
                    Expect.equal ( "something", [ "other" ] ) ( state.nextItem, items )
            , test "any nonspecial key means induces no changes" <|
                \_ ->
                    let
                        ( state, items, _ ) =
                            MultiInput.update updateConfig (MultiInput.KeyDown 4) { defaultInitState | nextItem = "something" } []
                    in
                    Expect.equal ( "something", [] ) ( state.nextItem, items )
            , test "removing an item" <|
                \_ ->
                    let
                        ( state, items, _ ) =
                            MultiInput.update updateConfig (MultiInput.RemoveItem "two") { defaultInitState | nextItem = "something" } [ "one", "two", "three" ]
                    in
                    Expect.equal ( "something", [ "one", "three" ] ) ( state.nextItem, items )
            , test "the current item is itemized when it loses focus" <|
                \_ ->
                    let
                        ( state, items, _ ) =
                            MultiInput.update updateConfig (MultiInput.TextareaBlurred "halfway") { defaultInitState | nextItem = "" } []
                    in
                    Expect.equal ( "", [ "halfway" ] ) ( state.nextItem, items )
            , test "no new items are added when the current item is empty" <|
                \_ ->
                    let
                        messages =
                            [ MultiInput.KeyDown 9
                            , MultiInput.TextareaBlurred ""
                            ]

                        updateWhenEmpty msg =
                            let
                                ( state, items, _ ) =
                                    MultiInput.update updateConfig msg { defaultInitState | nextItem = "" } []
                            in
                            ( state.nextItem, items )
                    in
                    all (Expect.equal ( "", [] ) << updateWhenEmpty) (\_ -> "Expected that no new items were created") messages
            , test "when the input changes all the items are itemized and druplicates dropped" <|
                \_ ->
                    let
                        nextInput =
                            "one two\tthree\nfour, five,six,,,seven eight\n\n\neight\nnine"

                        ( state, items, _ ) =
                            MultiInput.update updateConfig (MultiInput.InputChanged nextInput) { defaultInitState | nextItem = "" } [ "previous" ]

                        expectedItems =
                            ( "nine", [ "previous", "one", "two", "three", "four", "five", "six", "seven", "eight" ] )
                    in
                    Expect.equal expectedItems ( state.nextItem, items )
            ]
        ]


updateConfig : MultiInput.UpdateConfig
updateConfig =
    { separators = [ ",", " ", "\t", "\n" ] }


defaultInitState : MultiInput.State
defaultInitState =
    { nextItem = "", id = "id" }


all : (a -> Expectation) -> (a -> String) -> List a -> Expectation
all expectation message cases =
    case List.head cases of
        Just head ->
            if expectation head == Expect.pass then
                all expectation message (Maybe.withDefault [] <| List.tail cases)
            else
                Expect.fail <| message head

        Nothing ->
            Expect.pass

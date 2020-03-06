module ParserTest exposing (..)

import Expect
import Parser exposing (DeadEnd, Problem(..))
import Test exposing (Test, describe, test)


type alias TestingParser a =
    String -> Result (List DeadEnd) a


type alias ParserTests a =
    List (ParserTest a)


type alias Testing =
    String


type ParserTest a
    = ParserTestEq Testing a
    | ParserTestErr Testing


getTestingString : ParserTest a -> String
getTestingString ptest =
    case ptest of
        ParserTestEq testing _ ->
            testing

        ParserTestErr testing ->
            testing


compileTests : String -> ParserTests a -> TestingParser a -> Test
compileTests description tests testingParser =
    describe description <|
        List.indexedMap (addParserTest testingParser) tests


addParserTest : TestingParser a -> Int -> ParserTest a -> Test
addParserTest testingParser idx ptest =
    let
        testingString : String
        testingString =
            getTestingString ptest

        testTitle : String
        testTitle =
            toTestTitle idx testingString
    in
    test testTitle <|
        \_ ->
            testingString
                |> testingParser
                |> (case ptest of
                        ParserTestEq _ res ->
                            Expect.equal (Ok res)

                        ParserTestErr _ ->
                            Expect.err
                   )


toTestTitle : Int -> String -> String
toTestTitle idx title =
    "#" ++ String.fromInt (idx + 1) ++ " " ++ title

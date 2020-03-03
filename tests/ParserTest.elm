module ParserTest exposing (..)

import Expect
import Parser exposing (DeadEnd, Problem(..))
import Test exposing (Test, describe, test)


type TestExpect a
    = ResEq a
    | ResErr


type alias TestedParser a =
    String -> Result (List DeadEnd) a


type alias ParserTests a =
    List (ParserTest a)


type alias ParserTest a =
    { title : String
    , testing : String
    , expecting : TestExpect a
    }


compileTests : String -> ParserTests a -> TestedParser a -> Test
compileTests description tests testedParser =
    let
        toTestTitle : Int -> String -> String
        toTestTitle idx title =
            "#" ++ String.fromInt (idx + 1) ++ " " ++ title
    in
    describe description <|
        List.indexedMap
            (\idx { title, testing, expecting } ->
                test (toTestTitle idx title) <|
                    \_ ->
                        testing
                            |> testedParser
                            |> (case expecting of
                                    ResEq val ->
                                        Expect.equal (Ok val)

                                    ResErr ->
                                        Expect.err
                               )
            )
            tests

module BoldParserTest exposing (..)

--  import Markdown.Types exposing (Inline(..))

import Expect
import Markdown.Parsers.Inline exposing (..)
import Parser exposing (Problem(..), run)
import Test exposing (..)


suite : Test
suite =
    describe "Bold parsing"
        [ testBoldParser
        ]


testBoldParser : Test
testBoldParser =
    let
        runBoldParser =
            run boldParser
    in
    describe "Test bold text parser"
        [ test "**...** is bold text" <|
            \_ ->
                runBoldParser "**this is bold**"
                    |> Expect.equal (Ok (Bold [ Text "this is bold" ]))

        --
        , test "__...__ is bold text" <|
            \_ ->
                runBoldParser "__this is also bold__"
                    |> Expect.equal (Ok (Bold [ Text "this is also bold" ]))

        -- This should be also test for inlineParser
        , test "Some text" <|
            \_ ->
                runBoldParser "** not really**bold**"
                    |> Expect.equal (Ok (Text "** not really"))

        --
        , test "there should be no space after bold opening" <|
            \_ ->
                runBoldParser "** this is not bold**"
                    |> Expect.equal (Ok (Text "** this is not bold"))

        --
        , test "there should be no space before bold closing" <|
            \_ ->
                runBoldParser "**this is also not bold **"
                    |> Expect.equal (Ok (Text "**this is also not bold "))
        ]

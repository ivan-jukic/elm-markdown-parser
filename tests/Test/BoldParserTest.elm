module Test.BoldParserTest exposing (suite)

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

        -- This should be also test for inlineParser, but with different
        -- expected parsing result.
        , test "Bold parser should return Text if text after asterisks/underscores cannot be parsed as Bold" <|
            \_ ->
                runBoldParser "** not really**bold**"
                    |> Expect.equal (Ok (Text "** not really"))
        , test "Opening asterisks don't mean bold if there's space after" <|
            \_ ->
                runBoldParser "** this is not bold**"
                    |> Expect.equal (Ok (Text "** this is not bold**"))

        --
        , test "Opening underscores don't mean bold if there's space after" <|
            \_ ->
                runBoldParser "__ this is not bold__"
                    |> Expect.equal (Ok (Text "__ this is not bold__"))

        --
        , test "Closing asterisks don't mean bold if there's space before" <|
            \_ ->
                runBoldParser "**this is also not bold **"
                    |> Expect.equal (Ok (Text "**this is also not bold **"))

        --
        , test "Closing underscores don't mean bold if there's space before" <|
            \_ ->
                runBoldParser "__this is also not bold __"
                    |> Expect.equal (Ok (Text "__this is also not bold __"))

        -- The parser tries to find the closing ** bound, and it will chomp all
        -- characters until it finds it.
        , test "You cannot mix and match asterisks & underscores" <|
            \_ ->
                runBoldParser "**this is not bold__"
                    |> Expect.equal (Ok (Text "**this is not bold__"))

        --
        , test "You cannot mix and match underscores & asterisks" <|
            \_ ->
                runBoldParser "__this is not bold**"
                    |> Expect.equal (Ok (Text "__this is not bold**"))
        ]

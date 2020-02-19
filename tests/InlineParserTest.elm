module InlineParserTest exposing (..)

import Expect
import Markdown.Parsers.Inline exposing (..)
import Parser exposing (Problem(..), run)
import Test exposing (..)


suite : Test
suite =
    describe "Inline parsing"
        [ only testInlineParser
        ]


testInlineParser : Test
testInlineParser =
    let
        runInlineParser =
            run inlineParser
    in
    describe "Test inline content parser"
        [ test "Parse some crazy text" <|
            \_ ->
                runInlineParser "** not really**bold**"
                    |> Expect.equal (Ok [ Text "** not really", Bold [ Text "bold" ] ])
        ]

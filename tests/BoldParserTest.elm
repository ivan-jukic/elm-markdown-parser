module BoldParserTest exposing (..)

import Expect
import Markdown.Parsers.Inline exposing (..)
import Markdown.Types exposing (MarkdownInline(..))
import Parser exposing (run)
import Test exposing (..)


suite : Test
suite =
    describe "Bold parsing"
        [ only testBoldParser
        ]


testBoldParser : Test
testBoldParser =
    describe "Test bold text parser"
        [ test "** bold text" <|
            \_ ->
                "**this is bold**"
                    |> run (boldParser TwoStars)
                    |> Debug.log "bold?"
                    |> Expect.equal (Ok (Bold (Text "this is bold")))
        ]

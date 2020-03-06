module Test.BoldParserTest exposing (testBoldParser)

import Markdown.Parsers.Inline exposing (..)
import Parser exposing (run)
import ParserTest exposing (..)
import Test exposing (..)


{-| Bold parser is designed so it doesnt't fail. If it cannot parse something
as bold, it will return parsed text up until it encounters the first contextual
bound in the string, or the end of the string.
-}
testBoldParser : Test
testBoldParser =
    boldParser
        |> run
        |> compileTests "Test bold text parser"
            [ ParserTestEq
                "**this is bold**"
                (Bold [ Text "this is bold" ])

            --
            , ParserTestEq
                "__this is also bold__"
                (Bold [ Text "this is also bold" ])

            -- This should be also test for inlineParser, but with different
            -- expected result of parsing.
            , ParserTestEq
                "** not really**bold**"
                (Text "** not really")

            -- Opening asterisks don't mean bold if there's space after
            , ParserTestEq
                "** this is not bold**"
                (Text "** this is not bold**")

            -- Opening underscores don't mean bold if there's space after
            , ParserTestEq
                "__ this is not bold__"
                (Text "__ this is not bold__")

            -- Closing asterisks don't mean bold if there's a space before
            , ParserTestEq
                "**this is also not bold **"
                (Text "**this is also not bold **")

            -- Closing underscores don't mean bold if there's a space before
            , ParserTestEq
                "__this is also not bold __"
                (Text "__this is also not bold __")

            -- You cannot mix and match asterisks & underscores
            , ParserTestEq
                "**this is not bold__"
                (Text "**this is not bold__")

            -- You cannot mix and match underscores & asterisks
            , ParserTestEq
                "__this is not bold**"
                (Text "__this is not bold**")
            ]

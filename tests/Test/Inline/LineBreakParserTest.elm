module Test.Inline.LineBreakParserTest exposing (testLineBreakParser)

import Markdown.Parsers.Inline exposing (..)
import Parser exposing (run)
import ParserTest exposing (..)
import Test exposing (..)


{-| Valid line break is a sequence of two or more spaces, and a single new line.
-}
testLineBreakParser : Test
testLineBreakParser =
    lineBreakParser
        |> run
        |> compileTests "Test line brake parser on few examples"
            [ -- this is a valid line break
              ParserTestEq "  \n" LineBreak

            -- this is also valid line break
            , ParserTestEq "   \n" LineBreak

            -- this is also valid line break
            , ParserTestEq "       \n" LineBreak

            -- single space and new line are not valid line break
            , ParserTestErr " \n"

            -- two or more spaces, followed by multiple new lines are not br
            , ParserTestErr "  \n\n"

            -- tabs are also not valid in line breaks
            , ParserTestErr "\t\t\n"

            -- combination of tabs and spaces is also not valid
            , ParserTestErr "\t  \n"
            ]

module Test.Inline.ItalicParserTest exposing (testItalicParser)

import Markdown.Parsers.Inline exposing (..)
import Parser exposing (run)
import ParserTest exposing (..)
import Test exposing (..)


testItalicParser : Test
testItalicParser =
    italicParser
        |> run
        |> compileTests "Test italic text parser"
            [ ParserTestEq
                "*this is italic*"
                (Italic [ Text "this is italic" ])

            --
            , ParserTestEq
                "_this is also italic_"
                (Italic [ Text "this is also italic" ])

            -- Parser will parse from current underline to the first next one!
            -- The first next one might be opening italic underscore further in
            -- the text, so we should not consume it. This is valid for any
            -- italic/bold/ strikethrough parsing.
            , ParserTestEq
                "_ no space after underscore_"
                (Text "_ no space after underscore_")

            -- Opening asterisk doesn't mean italic if there's space after
            , ParserTestEq
                "* no space after asterisk*"
                (Text "* no space after asterisk*")

            -- Closing underscore doesn't mean italic if there's space before
            , ParserTestEq
                "_no space before underscore _"
                (Text "_no space before underscore _")

            -- Closing asterisk doesn't mean italic if there's space before
            , ParserTestEq
                "*no space before asterisk *"
                (Text "*no space before asterisk *")

            -- You cannot mix and match asterisk & underscore
            , ParserTestEq
                "*this is not italic_"
                (Text "*this is not italic_")

            -- You cannot mix and match underscore & asterisk
            , ParserTestEq
                "_this is not italic*"
                (Text "_this is not italic*")
            ]

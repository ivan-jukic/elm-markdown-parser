module ItalicParserTest exposing (..)

import Expect
import Markdown.Parsers.Inline exposing (..)
import Parser exposing (Problem(..), run)
import Test exposing (..)


suite : Test
suite =
    describe "Italic parsing"
        [ testItalicParser
        ]


testItalicParser : Test
testItalicParser =
    let
        runItalicParser =
            run italicParser
    in
    describe "Test italic text parser"
        [ test "*...* is italic text" <|
            \_ ->
                runItalicParser "*this is italic*"
                    |> Expect.equal (Ok (Italic [ Text "this is italic" ]))

        --
        , test "_..._ is italic text" <|
            \_ ->
                runItalicParser "_this is also italic_"
                    |> Expect.equal (Ok (Italic [ Text "this is also italic" ]))

        -- Parser will parse from current underline to the first next one!
        -- The first next one might be opening italic underscore further in the
        -- text, so we should not consume it. This is valid for any italic/bold/
        -- strikethrough parsing.
        , test "Opening underscore doesn't mean italic if there's space after" <|
            \_ ->
                runItalicParser "_ no space after underscore_"
                    |> Expect.equal (Ok (Text "_ no space after underscore"))

        --
        , test "Opening asterisk doesn't mean italic if there's space after" <|
            \_ ->
                runItalicParser "* no space after asterisk*"
                    |> Expect.equal (Ok (Text "* no space after asterisk"))

        --
        , test "Closing underscore doesn't mean italic if there's space before" <|
            \_ ->
                runItalicParser "_no space before underscore _"
                    |> Expect.equal (Ok (Text "_no space before underscore "))

        --
        , test "Closing asterisk doesn't mean italic if there's space before" <|
            \_ ->
                runItalicParser "*no space before asterisk *"
                    |> Expect.equal (Ok (Text "*no space before asterisk "))

        --
        , test "You cannot mix and match asterisk & underscore" <|
            \_ ->
                runItalicParser "*this is not italic_"
                    |> Expect.equal (Ok (Text "*this is not italic"))

        --
        , test "You cannot mix and match underscore & asterisk" <|
            \_ ->
                runItalicParser "_this is not italic*"
                    |> Expect.equal (Ok (Text "_this is not italic"))
        ]

module InlineParserTest exposing (..)

import Expect
import Markdown.Parsers.Inline exposing (..)
import Parser exposing (Problem(..), run)
import Test exposing (..)


suite : Test
suite =
    describe "Inline parsing"
        [ testInlineParser
        ]


testInlineParser : Test
testInlineParser =
    let
        runInlineParser =
            run inlineParser
    in
    describe "Test inline content parser"
        [ test "Inline parse some reulgar text with bold and italic elements" <|
            \_ ->
                runInlineParser "This is **bold**, this is _italic_ and this ~~strikethrough~~"
                    |> Expect.equal
                        (Ok
                            [ Text "This is "
                            , Bold [ Text "bold" ]
                            , Text ", this is "
                            , Italic [ Text "italic" ]
                            , Text " and this "
                            , Strikethrough [ Text "strikethrough" ]
                            ]
                        )

        --
        , test "Parse some crazy text" <|
            \_ ->
                runInlineParser "** not really all**bold**"
                    |> Expect.equal
                        (Ok
                            [ Text "** not really all"
                            , Bold [ Text "bold" ]
                            ]
                        )
        ]

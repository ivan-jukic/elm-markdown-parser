module Test.Inline.InlineParserTest exposing (testInlineParser)

import Markdown.Parsers.Inline exposing (..)
import Parser exposing (run)
import ParserTest exposing (..)
import Test exposing (..)


testInlineParser : Test
testInlineParser =
    inlineParser
        |> run
        |> compileTests "Test inline content parser"
            [ ParserTestEq
                "This is **bold**, this is _italic_ and this ~~strikethrough~~"
                [ Text "This is "
                , Bold [ Text "bold" ]
                , Text ", this is "
                , Italic [ Text "italic" ]
                , Text " and this "
                , Strikethrough [ Text "strikethrough" ]
                ]

            --
            , ParserTestEq
                "Nested **bold *italic***"
                [ Text "Nested "
                , Bold
                    [ Text "bold "
                    , Italic [ Text "italic" ]
                    ]
                ]

            --
            , ParserTestEq
                "Nested **bold _italic_ ~~strike~~**"
                [ Text "Nested "
                , Bold
                    [ Text "bold "
                    , Italic [ Text "italic" ]
                    , Text " "
                    , Strikethrough [ Text "strike" ]
                    ]
                ]

            --
            , ParserTestEq
                "** not really all**bold**"
                [ Text "** not really all"
                , Bold [ Text "bold" ]
                ]

            --
            , ParserTestEq
                "this ** is ** some ** non valid ** bounds and **one** valid"
                [ Text "this ** is ** some ** non valid ** bounds and "
                , Bold [ Text "one" ]
                , Text " valid"
                ]

            --
            , ParserTestEq
                "a bit of **nested *text***"
                [ Text "a bit of "
                , Bold
                    [ Text "nested "
                    , Italic [ Text "text" ]
                    ]
                ]

            --
            , ParserTestEq
                "this **is a bit _of mixup**_"
                [ Text "this "
                , Bold [ Text "is a bit _of mixup" ]
                , Text "_"
                ]

            --
            , ParserTestEq
                "this is *italic* but ~this~ is nothing"
                [ Text "this is "
                , Italic [ Text "italic" ]
                , Text " but ~this~ is nothing"
                ]
            ]

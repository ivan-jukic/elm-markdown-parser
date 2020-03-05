module Test.InlineParserTest exposing
    ( testAsterisksClosingBoundSearch
    , testInlineParser
    , testOpenBoundSearch
    , testUnderscoresClosingBoundSearch
    )

import Markdown.Parsers.Inline exposing (..)
import Parser exposing ((|=), Problem(..), run)
import ParserTest exposing (..)
import Test exposing (..)


{-| Parser which consumes all chars until it finds a context bound, but it does
not consume the context bound.
-}
boundSearchParser : TestedParser String
boundSearchParser =
    chompWhileNotOpenBound
        |> Parser.getChompedString
        |> run


testOpenBoundSearch : Test
testOpenBoundSearch =
    boundSearchParser
        |> compileTests "chompWhileNotBound parser consumes chars until a valid context bound is reached, but the bound is not consumed."
            [ ParserTest
                "Take chars until bound **and not after"
                (ResEq "Take chars until bound ")

            --
            , ParserTest
                "Take chars until bound ** and after if it's not valid"
                (ResEq "Take chars until bound ** and after if it's not valid")

            --
            , ParserTest
                "Skip special chars like (~) __but not context bound"
                (ResEq "Skip special chars like (~) ")

            --
            , ParserTest
                "~~Result should be empty string if context bound is at beginning"
                (ResEq "")

            -- Space after bound makes it invalid opening bound
            , ParserTest
                "~~ Result is not empty if the bound is not valid"
                (ResEq "~~ Result is not empty if the bound is not valid")
            ]



-- Inline search for closing bound!


{-| We'd want to know if the bound was properly found, and what was the
string that whas chomped, for testing purposes.
-}
closingBoundParser : String -> TestedParser ( String, ClosingBound )
closingBoundParser bound =
    Parser.run
        (Parser.succeed identity
            |= chompUntilClosingBound bound
            |> Parser.mapChompedString Tuple.pair
        )


testUnderscoresClosingBoundSearch : Test
testUnderscoresClosingBoundSearch =
    closingBoundParser "__"
        |> compileTests "chompUntilClosingBound where bound is (__)"
            [ ParserTest
                "Looking for underscores__"
                (ResEq ( "Looking for underscores__", BoundFound ))

            --
            , ParserTest
                "Bound is__ not at the end"
                (ResEq ( "Bound is__", BoundFound ))

            --
            , ParserTest
                "__ Bound at the beginning, but should be preceeded by chars."
                ResErr

            --
            , ParserTest
                "There's no* underscores bound** in this~~ string so throw err!"
                ResErr

            --
            , ParserTest "looking for underscores_" ResErr
            , ParserTest "looking for underscores**" ResErr
            ]


testAsterisksClosingBoundSearch : Test
testAsterisksClosingBoundSearch =
    closingBoundParser "**"
        |> compileTests "chompUntilClosingBound where bound is (**)"
            [ ParserTest
                "Looking for asterisks**"
                (ResEq ( "Looking for asterisks**", BoundFound ))

            --
            , ParserTest
                "Bound is** not at the end"
                (ResEq ( "Bound is**", BoundFound ))

            --
            , ParserTest
                "** Bound at the beginning, but should be preceeded by chars."
                ResErr

            --
            , ParserTest
                "There's no* double asterisk bound__ in this~~ string so throw err!"
                ResErr
            ]


testInlineParser : Test
testInlineParser =
    inlineParser
        |> run
        |> compileTests "Test inline content parser"
            [ ParserTest
                "This is **bold**, this is _italic_ and this ~~strikethrough~~"
                (ResEq
                    [ Text "This is "
                    , Bold [ Text "bold" ]
                    , Text ", this is "
                    , Italic [ Text "italic" ]
                    , Text " and this "
                    , Strikethrough [ Text "strikethrough" ]
                    ]
                )

            --
            , ParserTest
                "Nested **bold *italic***"
                (ResEq
                    [ Text "Nested "
                    , Bold
                        [ Text "bold "
                        , Italic [ Text "italic" ]
                        ]
                    ]
                )

            --
            , ParserTest
                "Nested **bold _italic_ ~~strike~~**"
                (ResEq
                    [ Text "Nested "
                    , Bold
                        [ Text "bold "
                        , Italic [ Text "italic" ]
                        , Text " "
                        , Strikethrough [ Text "strike" ]
                        ]
                    ]
                )

            --
            , ParserTest
                "** not really all**bold**"
                (ResEq
                    [ Text "** not really all"
                    , Bold [ Text "bold" ]
                    ]
                )

            --
            , ParserTest
                "this ** is ** some ** non valid ** bounds and **one** valid"
                (ResEq
                    [ Text "this ** is ** some ** non valid ** bounds and "
                    , Bold [ Text "one" ]
                    , Text " valid"
                    ]
                )

            --
            , ParserTest
                "a bit of **nested *text***"
                (ResEq
                    [ Text "a bit of "
                    , Bold
                        [ Text "nested "
                        , Italic [ Text "text" ]
                        ]
                    ]
                )

            --
            , ParserTest
                "this **is a bit _of mixup**_"
                (ResEq
                    [ Text "this "
                    , Bold [ Text "is a bit _of mixup" ]
                    , Text "_"
                    ]
                )

            --
            , ParserTest
                "this is *italic* but ~this~ is nothing"
                (ResEq
                    [ Text "this is "
                    , Italic [ Text "italic" ]
                    , Text " but ~this~ is nothing"
                    ]
                )
            ]

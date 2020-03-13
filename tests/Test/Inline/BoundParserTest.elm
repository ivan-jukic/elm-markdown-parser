module Test.Inline.BoundParserTest exposing
    ( testAsterisksClosingBoundSearch
    , testOpenBoundSearch
    , testUnderscoresClosingBoundSearch
    )

import Markdown.Parsers.Inline exposing (..)
import Parser exposing ((|=), run)
import ParserTest exposing (..)
import Test exposing (..)


{-| Parser which consumes all chars until it finds a context bound, but it does
not consume the context bound.
-}
boundSearchParser : TestingParser String
boundSearchParser =
    chompWhileNotOpenBound
        |> Parser.getChompedString
        |> run


testOpenBoundSearch : Test
testOpenBoundSearch =
    boundSearchParser
        |> compileTests "chompWhileNotBound parser consumes chars until a valid context bound is reached, but the bound is not consumed."
            [ ParserTestEq
                "Take chars until bound **and not after"
                "Take chars until bound "

            --
            , ParserTestEq
                "Take chars until bound ** and after if it's not valid"
                "Take chars until bound ** and after if it's not valid"

            --
            , ParserTestEq
                "Skip special chars like (~) __but not context bound"
                "Skip special chars like (~) "

            --
            , ParserTestEq
                "~~Result should be empty string if context bound is at beginning"
                ""

            -- Space after bound makes it invalid opening bound
            , ParserTestEq
                "~~ Result is not empty if the bound is not valid"
                "~~ Result is not empty if the bound is not valid"
            ]



-- Inline search for closing bound!


{-| We'd want to know if the bound was properly found, and what was the
string that whas chomped, for testing purposes.
-}
closingBoundParser : String -> TestingParser ( String, ClosingBound )
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
            [ ParserTestEq
                "Looking for underscores__"
                ( "Looking for underscores__", BoundFound )

            --
            , ParserTestEq
                "Bound is__ not at the end"
                ( "Bound is__", BoundFound )

            --
            , ParserTestErr "__ Bound at the beginning, but should be preceeded by chars."
            , ParserTestErr "There's no* underscores bound** in this~~ string so throw err!"
            , ParserTestErr "looking for underscores_"
            , ParserTestErr "looking for underscores**"
            ]


testAsterisksClosingBoundSearch : Test
testAsterisksClosingBoundSearch =
    closingBoundParser "**"
        |> compileTests "chompUntilClosingBound where bound is (**)"
            [ ParserTestEq
                "Looking for asterisks**"
                ( "Looking for asterisks**", BoundFound )

            --
            , ParserTestEq
                "Bound is** not at the end"
                ( "Bound is**", BoundFound )

            --
            , ParserTestErr "** Bound at the beginning, but should be preceeded by chars."
            , ParserTestErr "There's no* double asterisk bound__ in this~~ string so throw err!"
            ]

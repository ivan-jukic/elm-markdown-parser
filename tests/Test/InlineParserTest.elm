module Test.InlineParserTest exposing
    ( testBoundSearch
    , testInlineParser
    , testUnderscoresClosingBoundSearch
    )

import Expect
import Markdown.Parsers.Inline exposing (..)
import Parser exposing ((|=), Problem(..), run)
import ParserTest exposing (..)
import Test exposing (..)


{-| Parser which consumes all chars until it finds a context bound, but it does
not consume the context bound.
-}
boundSearchParser : TestedParser String
boundSearchParser =
    chompWhileNotBound
        |> Parser.getChompedString
        |> run


testBoundSearch : Test
testBoundSearch =
    boundSearchParser
        |> compileTests "chompWhileNotBound parser consumes chars until context bound is reached, but the bound is not consumed."
            [ ParserTest
                "Take chars until bound ** and not after"
                (ResEq "Take chars until bound ")

            --
            , ParserTest
                "Skip special chars like (~) __ but not context bound"
                (ResEq "Skip special chars like (~) ")

            --
            , ParserTest
                "~~ Result should be empty string if context bound is at beginning"
                (ResEq "")
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
            , ParserTest "looking for underscores*" ResErr
            , ParserTest "looking for underscores~~" ResErr
            , ParserTest "looking for underscores~" ResErr
            ]



-- describe "Test parser to find closing bounds while parsing inline content."
--     [ test "There's only one closing bound at the end of string" <|
--         \_ ->
--             "looking for underscores__"
--                 |> testParser "__"
--                 |> Expect.equal (Ok ( "looking for underscores__", BoundFound ))
--     --
--     , test "Closing bound is in the middle of the string, and should only be consumed up to that point" <|
--         \_ ->
--             "bound is** not at the end"
--                 |> testParser "**"
--                 |> Expect.equal (Ok ( "bound is**", BoundFound ))
--     --
--     , test "There are other special characters and bound symbols in string after end bound" <|
--         \_ ->
--             "There*is_other**chars in this string as~~well..**"
--                 |> testParser "**"
--                 |> Expect.equal (Ok ( "There*is_other**", BoundFound ))
--     --
--     , test "If there's no bound in string, return the whole string" <|
--         \_ ->
--             "There's no* end bound__ in this~~ string!"
--                 |> testParser "**"
--                 |> Expect.err
--     --
--     , test "Not a closing bound if there's whitespace before, it may be opening bound further in the string." <|
--         \_ ->
--             "There's whitespace before **this bound."
--                 |> testParser "**"
--                 |> Expect.err
--     ]


testInlineParser : Test
testInlineParser =
    let
        testParser =
            run inlineParser
    in
    describe "Test inline content parser"
        [ test "Inline parse some regular text with bold and italic elements" <|
            \_ ->
                testParser "This is **bold**, this is _italic_ and this ~~strikethrough~~"
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
        , test "Bold, italic, and strikethrough can nest as well (1)" <|
            \_ ->
                testParser "Nested **bold *italic***"
                    |> Expect.equal
                        (Ok
                            [ Text "Nested "
                            , Bold
                                [ Text "bold "
                                , Italic [ Text "italic" ]
                                ]
                            ]
                        )
        , test "Bold, italic, and strikethrough can nest as well (2)" <|
            \_ ->
                testParser "Nested **bold _italic_ ~~strike~~**"
                    |> Expect.equal
                        (Ok
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
        , test "Parse some crazy text" <|
            \_ ->
                testParser "** not really all**bold**"
                    |> Expect.equal
                        (Ok
                            [ Text "** not really all"
                            , Bold [ Text "bold" ]
                            ]
                        )

        --
        , test "Non-valid consecutive context bounds" <|
            \_ ->
                testParser "this ** is ** some ** non valid ** bounds and **one** valid"
                    |> Expect.equal
                        (Ok
                            [ Text "this ** is ** some ** non valid ** bounds and "
                            , Bold [ Text "one" ]
                            , Text " valid"
                            ]
                        )

        --
        , test "Nested bold and italic text defined with asterisks" <|
            \_ ->
                testParser "a bit of **nested *text***"
                    |> Expect.equal
                        (Ok
                            [ Text "a bit of "
                            , Bold
                                [ Text "nested "
                                , Italic [ Text "text" ]
                                ]
                            ]
                        )

        --
        , test "Mixed up non valid bold and italic" <|
            \_ ->
                testParser "this **is a bit _of mixup**_"
                    |> Expect.equal
                        (Ok
                            [ Text "this "
                            , Bold [ Text "is a bit _of mixup" ]
                            , Text "_"
                            ]
                        )

        --
        , test "Single tilde (~) does not define any context" <|
            \_ ->
                testParser "this is *italic* but ~this~ is nothing"
                    |> Expect.equal
                        (Ok
                            [ Text "this is "
                            , Italic [ Text "italic" ]
                            , Text " but ~this~ is nothing"
                            ]
                        )
        ]

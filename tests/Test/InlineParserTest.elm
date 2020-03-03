module Test.InlineParserTest exposing
    ( testBoundSearch
    , testInlineLineChomping
    , testInlineParser
    )

import Expect
import Markdown.Parsers.Inline exposing (..)
import Parser exposing ((|=), Problem(..), run)
import ParserTest exposing (..)
import Test exposing (..)


testBoundSearch : Test
testBoundSearch =
    let
        testParser =
            chompWhileNotBound
                |> Parser.getChompedString
                |> run
    in
    testParser
        |> compileTests "Test chompWhileNotBound parser"
            [ ParserTest "Should find the (**) context bound"
                "finding** a bound"
                (ResEq "finding")

            --
            , ParserTest "Should skip the (~) and find (__) context bound"
                "skip ~ and find __ bound"
                (ResEq "skip ~ and find ")

            --
            , ParserTest "Should just return empty string if context bound is at beginning"
                "~~ result should be empty string"
                (ResEq "")
            ]


{-| Test line chomping after stumbiling upon parsing an opening bound (eg. \*\*
or \_ or ~~), unil the same closing symbol is found or new line, or the end
of the string we're parsing.
-}
testInlineLineChomping : Test
testInlineLineChomping =
    let
        -- We'd want to know if the bound was properly found, and what was the
        -- string that whas chomped.
        testParser : String -> String -> Result (List Parser.DeadEnd) ( String, ClosingBound )
        testParser bound =
            Parser.run
                (Parser.succeed identity
                    |= chompUntilClosingBound bound
                    |> Parser.mapChompedString Tuple.pair
                )
    in
    describe "Test parser to find closing bounds while parsing inline content."
        [ test "There's only one closing bound at the end of string" <|
            \_ ->
                "looking for underscores__"
                    |> testParser "__"
                    |> Expect.equal (Ok ( "looking for underscores__", BoundFound ))

        --
        , test "Closing bound is in the middle of the string, and should only be consumed up to that point" <|
            \_ ->
                "bound is** not at the end"
                    |> testParser "**"
                    |> Expect.equal (Ok ( "bound is**", BoundFound ))

        --
        , test "There are other special characters and bound symbols in string after end bound" <|
            \_ ->
                "There*is_other**chars in this string as~~well..**"
                    |> testParser "**"
                    |> Expect.equal (Ok ( "There*is_other**", BoundFound ))

        --
        , test "If there's no bound in string, return the whole string" <|
            \_ ->
                "There's no* end bound__ in this~~ string!"
                    |> testParser "**"
                    |> Expect.err

        --
        , test "Not a closing bound if there's whitespace before, it may be opening bound further in the string." <|
            \_ ->
                "There's whitespace before **this bound."
                    |> testParser "**"
                    |> Expect.err
        ]


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
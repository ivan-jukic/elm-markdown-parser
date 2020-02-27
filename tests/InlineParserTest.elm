module InlineParserTest exposing (..)

import Expect
import Markdown.Parsers.Inline as Inline exposing (..)
import Parser exposing (Problem(..), run)
import Test exposing (..)


suite : Test
suite =
    describe "Inline parsing"
        [ testInlineParser

        --, testInlineLineChomping
        ]


{-| Test line chomping after stumbiling upon parsing an opening bound (eg. \*\*
or \_ or ~~), unil the same closing symbol is found or new line, or the end
of the string we're parsing.
-}



-- testInlineLineChomping : Test
-- testInlineLineChomping =
--     let
--         testParser =
--             run << chompUntilNewLineEndOrClosingBound
--     in
--     describe "Test parser to find closing bounds while parsing inline content."
--         [ test "There's only one closing bound at the end of string" <|
--             \_ ->
--                 "looking for underscores__"
--                     |> testParser "__"
--                     |> Expect.equal (Ok "looking for underscores__"))
--         --
--         , test "Closing bound is in the middle of the string, and should only be consumed up to that point" <|
--             \_ ->
--                 "bound is** not at the end"
--                     |> testParser "**"
--                     |> Expect.equal (Ok "bound is**")
--         --
--         , test "There are other special characters and bound symbols in string after end bound" <|
--             \_ ->
--                 "There*is_other**chars in this string as~~well..**"
--                     |> testParser "**"
--                     |> Expect.equal (Ok "There*is_other**")
--         --
--         , test "If there's no bound in string, return the whole string" <|
--             \_ ->
--                 "There's no* end bound__ in this~~ string!"
--                     |> testParser "**"
--                     |> Expect.equal (Ok "There's no* end bound__ in this~~ string!")
--         --
--         , test "Not a closing bound if there's whitespace before, it may be opening bound further in the string." <|
--             \_ ->
--                 "There's whitespace before **this bound."
--                     |> testParser "**"
--                     |> Expect.equal (Ok "There's whitespace before **this bound.")
--         ]


testInlineParser : Test
testInlineParser =
    let
        testParser =
            run inlineParser
    in
    describe "Test inline content parser"
        [ test "Inline parse some reulgar text with bold and italic elements" <|
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
        , test "Bold, italic, and strikethrough can nest as well" <|
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
        ]

module BoldParserTest exposing (..)

import Expect
import Markdown.Parsers.Inline exposing (..)
import Markdown.Types exposing (Inline(..))
import Parser exposing (Problem(..), run)
import Test exposing (..)


suite : Test
suite =
    describe "Bold parsing"
        [ only testBoldParser
        ]


starBoldParser : String -> Result (List Parser.DeadEnd) Inline
starBoldParser =
    run (boldParser TwoStars)


underlineBoldParser : String -> Result (List Parser.DeadEnd) Inline
underlineBoldParser =
    run (boldParser TwoUnderlines)


testBoldParser : Test
testBoldParser =
    describe "Test bold text parser"
        [ test "**...** is bold text" <|
            \_ ->
                starBoldParser "**this is bold**"
                    |> Expect.equal (Ok (Bold [ Text "this is bold" ]))

        --
        , test "__...__ is bold text" <|
            \_ ->
                underlineBoldParser "__this is also bold__"
                    |> Expect.equal (Ok (Bold [ Text "this is also bold" ]))

        --
        , test "there should be no space after bold opening" <|
            \_ ->
                starBoldParser "** this is err**"
                    |> Expect.equal (Err [ { col = 3, problem = UnexpectedChar, row = 1 } ])

        --
        , test "there should be no space before bold closing" <|
            \_ ->
                starBoldParser "**this is also err **"
                    |> Expect.equal (Err [ { col = 20, problem = UnexpectedChar, row = 1 } ])
        ]

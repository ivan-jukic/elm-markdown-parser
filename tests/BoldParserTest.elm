module BoldParserTest exposing (..)

--  import Markdown.Types exposing (Inline(..))

import Expect
import Markdown.Parsers.Inline exposing (..)
import Parser exposing (Problem(..), run)
import Test exposing (..)


suite : Test
suite =
    describe "Bold parsing"
        [ only testBoldParser
        ]


testBoldParser : Test
testBoldParser =
    let
        runBoldParser =
            run boldParser
    in
    describe "Test bold text parser"
        [ test "**...** is bold text" <|
            \_ ->
                run inlineParser "**this is bold**"
                    |> Debug.log "1st bold"
                    |> Expect.equal (Ok [ Bold [ Text "this is bold" ] ])

        -- --
        -- , test "__...__ is bold text" <|
        --     \_ ->
        --         runBoldParser "__this is also bold__"
        --             |> Expect.equal (Ok (Bold [ Text "this is also bold" ]))
        -- --
        -- , test "there should be no space after bold opening" <|
        --     \_ ->
        --         runBoldParser "** this is not bold**"
        --             |> Expect.equal (Ok (Text "** this is not bold"))
        -- --
        -- , test "there should be no space before bold closing" <|
        --     \_ ->
        --         runBoldParser "**this is also err **"
        --             |> Expect.equal (Err [ { col = 20, problem = UnexpectedChar, row = 1 } ])
        ]



{-
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
-}

module UnorderedListParserTest exposing (..)

import Expect
import Markdown.Parsers.List exposing (ulItemParser, ulParser)
import Markdown.Types exposing (ListItem(..), MarkdownList(..))
import Parser exposing (run)
import Test exposing (..)


md : String
md =
    "- Ul first item"


suite : Test
suite =
    describe "Test unordered list parsing"
        [ testListItemParser
        , testNonValidListItems
        , testFullListParser
        ]


testListItemParser : Test
testListItemParser =
    describe "Test various valid list items"
        [ test "Test list item with '-' indicator" <|
            \_ ->
                "- Hyphen li item"
                    |> run ulItemParser
                    |> Expect.equal (Ok (ListItem "Hyphen li item"))
        , test "Test list item with '+' indicator" <|
            \_ ->
                "+ Plus li item"
                    |> run ulItemParser
                    |> Expect.equal (Ok (ListItem "Plus li item"))
        , test "Test list item with '*' indicator" <|
            \_ ->
                "* Asterisk li item"
                    |> run ulItemParser
                    |> Expect.equal (Ok (ListItem "Asterisk li item"))
        ]


testNonValidListItems : Test
testNonValidListItems =
    describe "Test various invalid list items"
        [ test "Missing space after '-' indicator" <|
            \_ ->
                "-Invalid hyphen li item"
                    |> run ulItemParser
                    |> Expect.err
        , test "Missing space after '+' indicator" <|
            \_ ->
                "+Invalid plus li item"
                    |> run ulItemParser
                    |> Expect.err
        , test "Missing space after '*' indicator" <|
            \_ ->
                "*Invalid asterisk li item"
                    |> run ulItemParser
                    |> Expect.err
        ]


testFullListParser : Test
testFullListParser =
    describe "Testing full list parsing"
        [ test "Full list" <|
            \_ ->
                "- first\n+ second\n* third"
                    |> run ulParser
                    |> Expect.equal
                        (Ok <|
                            UnorderedList
                                [ ListItem "first"
                                , ListItem "second"
                                , ListItem "third"
                                ]
                        )
        ]

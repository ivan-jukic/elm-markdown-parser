module UnorderedListParserTest exposing (..)

import Expect exposing (Expectation)
import Markdown.Parsers.List exposing (ulParser)
import Parser exposing (run)
import Test exposing (..)


md : String
md =
    """
- Ul first item
+ Ul second item
* Ul third item    
"""


suite : Test
suite =
    describe "Test unordered list parsing"
        [ testSimpleList
        ]


testSimpleList : Test
testSimpleList =
    test "Test simple unordered list with different li start indicators" <|
        \_ ->
            run ulParser md |> Expect.ok

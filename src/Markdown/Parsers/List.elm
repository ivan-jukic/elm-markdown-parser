module Markdown.Parsers.List exposing (..)

import Char.Parsers exposing (chompIfSpaceOrTab, chompOnlyOneNewLine)
import Markdown.Parsers.TextLine exposing (textLineParser)
import Markdown.Types exposing (..)
import Parser exposing (..)


type alias ListItemsStep =
    Step ListItems ListItems


{-| Attempt to parse list, list item by item, in a loop.
-}
ulParser : Parser MarkdownList
ulParser =
    map UnorderedList <|
        loop [] ulItemsParser


ulItemsParser : ListItems -> Parser ListItemsStep
ulItemsParser reversedItems =
    oneOf
        [ itemSucceed reversedItems |= ulItemParser

        -- If the previous parser does not start chomping, and fails, this one
        -- succeeds instead!
        , defaultSucceed reversedItems
        ]


ulItemParser : Parser ListItem
ulItemParser =
    succeed ListItem
        |. oneOf
            [ symbol "-"
            , symbol "+"
            , symbol "*"
            ]
        -- at least one space/tab must be present
        |. chompIfSpaceOrTab
        |. spaces
        |= textLineParser


olParser : Parser MarkdownList
olParser =
    problem "TODO not implemented"


itemSucceed : ListItems -> Parser (ListItem -> ListItemsStep)
itemSucceed reversedItems =
    succeed (\newItem -> Loop (newItem :: reversedItems))


defaultSucceed : ListItems -> Parser ListItemsStep
defaultSucceed reversedItems =
    succeed () |> map (\_ -> Done (List.reverse reversedItems))

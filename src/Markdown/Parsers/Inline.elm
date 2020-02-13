module Markdown.Parsers.Inline exposing (..)

import Markdown.Parsers.TextLine exposing (textLineParser)
import Markdown.Types exposing (..)
import Parser exposing (..)



-- BOLD PARSER


type BoldBounds
    = TwoStars
    | TwoUnderlines


boldSymbol : BoldBounds -> Parser ()
boldSymbol bounds =
    symbol <|
        case bounds of
            TwoStars ->
                "**"

            TwoUnderlines ->
                "__"


boldParser : BoldBounds -> Parser MarkdownInline
boldParser bounds =
    succeed Bold
        |. boldSymbol bounds
        |= textParser
        |. boldSymbol bounds



-- INLINE TEXT PARSER


textParser : Parser MarkdownInline
textParser =
    chompWhile Char.isAlphaNum
        |> getChompedString
        |> map Text

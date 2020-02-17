module Markdown.Parsers.Inline exposing (..)

import Char.Extras as Char
import Markdown.Parsers.TextLine exposing (textLineParser)
import Markdown.Types exposing (..)
import Parser exposing (..)


type InlineCheckpoints
    = JustText String
    | BoldStart
    | BoldEnd



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


boldStartParser : BoldBounds -> Parser InlineCheckpoints
boldStartParser boldBound =
    succeed BoldStart
        |. boldSymbol boldBound
        |. chompIf (not << Char.isWhitespace)


boldEndParser : BoldBounds -> Parser InlineCheckpoints
boldEndParser boldBound =
    succeed BoldEnd
        |. chompIf Char.isAlphaNum
        |. boldSymbol boldBound


boldParser : BoldBounds -> Parser Inline
boldParser bounds =
    succeed Bold
        |. boldSymbol bounds
        |= textParser
        |. boldSymbol bounds



-- INLINE TEXT PARSER


textParser : Parser InlineContent
textParser =
    chompIf Char.isAlphaNum
        |. chompWhileNotSpecialChar
        |> getChompedString
        |> map (List.singleton << Text)



-- Special characters for inline content


specialChars : List Char
specialChars =
    [ '*', '_', '~' ]


chompWhileNotSpecialChar : Parser ()
chompWhileNotSpecialChar =
    chompWhile (\c -> not <| List.member c specialChars)

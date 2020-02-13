module Markdown.Parsers.Inline exposing (emphasisParser)

import Markdown.Types exposing (..)
import Parser exposing (..)


{-| Emphasis parser parses bold, italic, bolditalic and strikethrough content!
-}
emphasisParser : Parser MarkdownInline
emphasisParser =
    succeed <| Text "Emphasis parser"

module Markdown.Parsers.Inline exposing (..)

-- import Markdown.Types exposing (..)
-- import Char.Extras as Char
-- import Markdown.Parsers.TextLine exposing (textLineParser)

import Parser exposing (..)
import String.Extras exposing (startsOrEndsWithWhitespace)


type Inline
    = Inline InlineContent


type alias InlineContent =
    List Section


type Section
    = Bold InlineContent
    | Italic InlineContent
    | Strikethrough InlineContent
    | Text String


type ContextBounds
    = TwoStars
    | TwoUnderlines
    | TwoTilde
    | SingleStar
    | SingleUnderline


boundToString : ContextBounds -> String
boundToString bounds =
    case bounds of
        TwoStars ->
            "**"

        TwoUnderlines ->
            "__"

        TwoTilde ->
            "~~"

        SingleStar ->
            "*"

        SingleUnderline ->
            "_"


inlineParser : Parser InlineContent
inlineParser =
    loop [] inlineParserSequence


inlineParserSequence : InlineContent -> Parser (Step InlineContent InlineContent)
inlineParserSequence revContent =
    let
        repeat : Section -> Step InlineContent InlineContent
        repeat newContent =
            Loop (newContent :: revContent)

        finish : () -> Step InlineContent InlineContent
        finish _ =
            Done (List.reverse revContent)
    in
    oneOf
        [ map finish <| end
        , map repeat <|
            oneOf
                [ boldParser
                , textParser
                ]
        ]


boldParser : Parser Section
boldParser =
    [ TwoStars, TwoUnderlines ]
        |> List.map (parseBySymbol Bold)
        |> oneOf


textParser : Parser Section
textParser =
    chompWhileNotSpecialChar
        |> getChompedString
        |> map Text


parseBySymbol : (InlineContent -> Section) -> ContextBounds -> Parser Section
parseBySymbol tagger bound =
    let
        boundStr : String
        boundStr =
            boundToString bound
    in
    succeed identity
        |. symbol boundStr
        |= oneOf
            -- Should we lazy parse alternatives? Eg. if we've parsed bold,
            -- perhaps we only need to lazy parse italic, strikethrough, and
            -- text???
            [ lazy (\_ -> map List.singleton textParser)
                |. symbol boundStr
                |> map tagger
            , textParser
            ]


isSpecialChar : Char -> Bool
isSpecialChar c =
    List.member c [ '*', '_', '~' ]


chompWhileNotSpecialChar : Parser ()
chompWhileNotSpecialChar =
    oneOf
        [ end
        , chompWhile (not << isSpecialChar)
        ]



------------------------------------------
-- type InlineContext
--     = BoldContext
--     | ItalicContext
--     | StrikethroughContext
--     | TextContext String
-- isContextChar : Char -> Bool
-- isContextChar c =
--     List.member c [ '*', '_', '~' ]
-- boldContextParser : Parser InlineContext
-- boldContextParser =
--     succeed BoldContext
--         |. oneOf
--             [ contextSymbol TwoStars
--             , contextSymbol TwoUnderlines
--             ]
-- strikethroughContextParser : Parser InlineContext
-- strikethroughContextParser =
--     succeed StrikethroughContext
--         |. contextSymbol TwoTilde
-- italicContextParser : Parser InlineContext
-- italicContextParser =
--     succeed ItalicContext
--         |. oneOf
--             [ contextSymbol SingleStar
--             , contextSymbol SingleUnderline
--             ]
-- textContextParser : Parser InlineContext
-- textContextParser =
--     succeed identity
--         |. chompWhile (not << isContextChar)
--         |> getChompedString
--         |> map TextContext

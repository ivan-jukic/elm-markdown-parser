module Markdown.Parsers.Inline exposing (..)

-- import Markdown.Types exposing (..)
-- import Markdown.Parsers.TextLine exposing (textLineParser)

import Char.Extras as Char
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
                , strikethroughParser
                , italicParser
                , textParser
                ]
        ]


boldParser : Parser Section
boldParser =
    [ TwoStars, TwoUnderlines ]
        |> List.map (parseBySymbol Bold)
        |> oneOf


italicParser : Parser Section
italicParser =
    [ SingleStar, SingleUnderline ]
        |> List.map (parseBySymbol Italic)
        |> oneOf


strikethroughParser : Parser Section
strikethroughParser =
    parseBySymbol Strikethrough TwoTilde


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

        asText : String -> Section
        asText =
            Text << (++) boundStr

        -- Run parser on the subcontent of the bold item!
        parseSubcontent : String -> Result (List DeadEnd) InlineContent
        parseSubcontent =
            String.replace boundStr "" >> run inlineParser

        runSubContentParsing : String -> Section
        runSubContentParsing chomped =
            tagger <|
                case parseSubcontent chomped of
                    Ok inlineSections ->
                        inlineSections

                    Err _ ->
                        -- Some error, tho this shouldn't happen, as the
                        -- parsers should always succeed!
                        [ asText chomped ]

        parseSubContent : String -> Parser Section
        parseSubContent chomped =
            if startsOrEndsWithWhitespace chomped then
                -- There's a whitespace at one end of the chopmed string, which
                -- means it's not italic/bold/strikethrough, so we treat it as
                -- text.
                succeed (asText chomped)

            else
                oneOf
                    [ -- If the comped string is followed by the closing bound
                      -- symbol, parse subcontent.
                      succeed identity
                        |. symbol boundStr
                        |> map (\_ -> runSubContentParsing chomped)

                    -- There's no closing bound symbol, so succeed as text!
                    , succeed (asText chomped)
                    ]
    in
    succeed identity
        |. symbol boundStr
        |= oneOf
            [ succeed identity
                |. chompIf (not << Char.isWhitespace)
                |. chompWhileNotSpecialChar
                |> getChompedString
                -- At this point we know that the bound symbol does not have a
                -- whitespace after, but still don't know if there's a
                -- whitespace before the next special character.
                |> andThen parseSubContent

            -- If there's whitespace after the bound symbol, parse as regular
            -- text, this is basically fallback so that the parser does not
            -- fail!
            , chompWhileNotSpecialChar
                |> getChompedString
                -- preserve initially chomped bounding symbol
                |> map (Text << (++) boundStr)
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

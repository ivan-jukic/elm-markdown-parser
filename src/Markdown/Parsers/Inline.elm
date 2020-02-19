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


italicParser : Parser Section
italicParser =
    [ SingleStar, SingleUnderline ]
        |> List.map (parseBySymbol Italic)
        |> oneOf


textParser : Parser Section
textParser =
    chompWhileNotSpecialChar
        |> getChompedString
        |> map Text


type ParseStatus
    = ByBound
    | ByText


parseBySymbol : (InlineContent -> Section) -> ContextBounds -> Parser Section
parseBySymbol tagger bound =
    let
        boundStr : String
        boundStr =
            boundToString bound

        -- Run parser on the subcontent of the bold item!
        parseSubcontent : String -> Result (List DeadEnd) InlineContent
        parseSubcontent =
            String.replace boundStr ""
                >> run inlineParser

        toContentByStatus : ( String, ParseStatus ) -> Section
        toContentByStatus ( chomped, status ) =
            let
                hasWhitespace =
                    startsOrEndsWithWhitespace chomped

                asText =
                    Text (boundStr ++ chomped)
            in
            case ( hasWhitespace, status ) of
                ( False, ByBound ) ->
                    tagger <|
                        case parseSubcontent chomped of
                            Ok inlineSections ->
                                inlineSections

                            Err _ ->
                                -- Some error, tho this shouldn't happen, as the
                                -- parsers should always succeed!
                                [ asText ]

                _ ->
                    asText
    in
    succeed Tuple.pair
        |. symbol boundStr
        |= (chompWhileNotSpecialChar
                |> getChompedString
           )
        |= oneOf
            [ succeed ByBound |. symbol boundStr
            , succeed ByText
            ]
        |> map toContentByStatus


isSpecialChar : Char -> Bool
isSpecialChar c =
    List.member c [ '*', '_', '~' ]


chompWhileNotSpecialChar : Parser ()
chompWhileNotSpecialChar =
    oneOf
        [ end
        , chompWhile (not << isSpecialChar)
        ]

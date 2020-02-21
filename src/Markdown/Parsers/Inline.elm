module Markdown.Parsers.Inline exposing (..)

-- import Markdown.Types exposing (..)

import Char.Extras as Char
import Markdown.Parsers.TextLine exposing (textLineParser)
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
            let
                boundLen =
                    String.length boundStr

                isClosingBoundFound =
                    String.left boundLen chomped == boundStr
            in
            succeed <|
                -- Check if the bound was found and is at the end of the string!
                if isClosingBoundFound then
                    -- Drop closing bound, and parse the content inside
                    chomped
                        |> String.dropRight boundLen
                        |> runSubContentParsing

                else
                    asText chomped
    in
    succeed identity
        |. symbol boundStr
        |= oneOf
            [ succeed identity
                |. chompIf (not << Char.isWhitespace)
                |. chompUntilNewLineEndOrClosingBound boundStr
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


chompUntilFirstBound : List String -> Parser String
chompUntilFirstBound bounds =
    



{-| The idea behind this parser is to lazily consume character by character
until we find the symbol bound that we're looking for, and then return all of
the consumed characters
-}
chompUntilNewLineEndOrClosingBound : String -> Parser String
chompUntilNewLineEndOrClosingBound sym =
    let
        lazySelfParser : Parser String
        lazySelfParser =
            lazy (\_ -> chompUntilNewLineEndOrClosingBound sym)
    in
    oneOf
        [ backtrackable (closingBoundParser sym)
        , end

        -- chomp char and lazy try again
        , chompIf (always True)
            |. lazySelfParser
        ]
        |> andThen commit
        |> getChompedString


closingBoundParser : String -> Parser ()
closingBoundParser sym =
    let
        isValidSymbol : Bool -> Parser ()
        isValidSymbol hasTrailingSpecialChar =
            if hasTrailingSpecialChar then
                -- Although this parser can produce error, it should be used
                -- so that it's backtrackable, and this error is not raised!
                problem "Unexpected special character found while parsing!"

            else
                commit ()
    in
    succeed identity
        |. chompIf (not << Char.isWhitespace)
        |. symbol sym
        |= oneOf
            [ backtrackable (chompIf isSpecialChar)
                |> map (always True)
            , succeed False
            ]
        |> andThen isValidSymbol



-- Special characters


isSpecialChar : Char -> Bool
isSpecialChar c =
    List.member c [ '*', '_', '~' ]


chompWhileNotSpecialChar : Parser ()
chompWhileNotSpecialChar =
    oneOf
        [ end
        , chompWhile (not << isSpecialChar)
        ]

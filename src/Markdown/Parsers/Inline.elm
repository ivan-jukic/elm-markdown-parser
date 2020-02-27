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

        boundLen : Int
        boundLen =
            String.length boundStr

        asText : String -> Section
        asText =
            Text << (++) boundStr

        runSubContentParsing : String -> Section
        runSubContentParsing chomped =
            chomped
                -- |> String.dropRight boundLen
                |> run inlineParser
                |> Result.withDefault [ asText chomped ]
                |> tagger

        parseSubContent : ( String, ClosingBound ) -> Parser Section
        parseSubContent ( chomped, boundFound ) =
            let
                _ =
                    Debug.log "chomped" ( chomped, boundFound )
            in
            succeed <|
                case boundFound of
                    BoundFound ->
                        -- remove bounds, and parse the sub content
                        chomped
                            |> String.dropLeft boundLen
                            |> String.dropRight boundLen
                            |> runSubContentParsing

                    BoundNotFound ->
                        Text chomped
    in
    succeed identity
        |. symbol boundStr
        |= oneOf
            [ -- 1st PATH!
              -- If there's no whitespace after the bound symbol, chomp all the
              -- chars until the closing bound is found, or we reach the end of
              -- the string.
              succeed identity
                |. chompIf (not << Char.isWhitespace)
                |= chompUntilNewLineEndOrClosingBound boundStr

            -- 2nd PATH!
            -- If there's whitespace after the bound symbol, parse as regular
            -- text, this is basically fallback so that the parser does not
            -- fail!
            , chompWhileNotSpecialChar
                |> map (always BoundNotFound)
            ]
        |> mapChompedString Tuple.pair
        |> andThen parseSubContent


type ClosingBound
    = BoundFound
    | BoundNotFound


{-| The idea behind this parser is to lazily consume character by character
until we find the symbol bound that we're looking for, and then return all of
the consumed characters
-}
chompUntilNewLineEndOrClosingBound : String -> Parser ClosingBound
chompUntilNewLineEndOrClosingBound bound =
    oneOf
        [ closingBoundParser bound
            |> backtrackable
            |> map (always BoundFound)
        , end
            |> map (always BoundNotFound)
        , succeed identity
            |. chompIf (always True)
            |= lazy (\_ -> chompUntilNewLineEndOrClosingBound bound)
        ]
        |> andThen commit


{-| Parser for matching closing bound!

    Closing bound should no be preceeded with a whitespace, and after the bound
    there should be no immediate special characters.

-}
closingBoundParser : String -> Parser ()
closingBoundParser bound =
    let
        -- Although this parser can produce error, it should be used so that
        -- it's backtrackable, and this error is not raised!
        isValidSymbol : Bool -> Parser ()
        isValidSymbol hasTrailingSpecialChar =
            if hasTrailingSpecialChar then
                problem "Unexpected special character found while parsing!"

            else
                commit ()
    in
    succeed identity
        |. chompIf (not << Char.isWhitespace)
        |. symbol bound
        |= oneOf
            [ -- If there's another special char after the bound, backtrack!
              chompIf isSpecialChar
                |> backtrackable
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

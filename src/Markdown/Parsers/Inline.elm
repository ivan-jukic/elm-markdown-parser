module Markdown.Parsers.Inline exposing (..)

-- import Markdown.Types exposing (..)

import Char.Extras as Char
import Parser exposing (..)


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


boundsList : List ContextBounds
boundsList =
    [ TwoStars
    , TwoUnderlines
    , TwoTilde
    , SingleStar
    , SingleUnderline
    ]


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
        |> map (mergeConsecutiveText [])


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
                [ -- TODO plug in here link, image, emoji and inline code parsers!
                  boldParser
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
    chompWhileNotBound
        |> getChompedString
        |> map Text


{-| A type used as a success indicator for the parsers that depend on finding
the closing bound of some content.
-}
type ClosingBound
    = BoundFound
    | BoundNotFound


parseBySymbol : (InlineContent -> Section) -> ContextBounds -> Parser Section
parseBySymbol tagger bound =
    let
        boundStr : String
        boundStr =
            boundToString bound

        boundLen : Int
        boundLen =
            String.length boundStr

        parseSubContent : ( String, ClosingBound ) -> Section
        parseSubContent ( chomped, boundFound ) =
            case boundFound of
                BoundFound ->
                    -- Drop bounds, and run the string through the parser to
                    -- parse the subcontent, as some things can be nested.
                    -- If it fails, just return it as a text section.
                    chomped
                        |> String.dropLeft boundLen
                        |> String.dropRight boundLen
                        |> run inlineParser
                        |> Result.withDefault [ Text chomped ]
                        |> tagger

                BoundNotFound ->
                    Text chomped
    in
    succeed identity
        |. symbol boundStr
        |= oneOf
            [ -- 1st PATH!
              -- If there's no whitespace after the bound symbol, chomp all the
              -- chars until the closing bound is found! If this parser fails
              -- the next one will be tried!
              backtrackable
                (succeed identity
                    |. chompIf (not << Char.isWhitespace)
                    |= chompUntilClosingBound boundStr
                )

            -- 2nd PATH!
            -- If there's whitespace after the bound symbol, parse as regular
            -- text, this is basically fallback so that the parser does not
            -- fail!
            , chompWhileNotSpecialChar
                |> map (always BoundNotFound)
            ]
        |> mapChompedString Tuple.pair
        |> andThen (commit << parseSubContent)


{-| Chomps a string until it finds a closing bound. If there's no closing bound
it will chomp the string until the end, and then return a problem.
-}
chompUntilClosingBound : String -> Parser ClosingBound
chompUntilClosingBound bound =
    oneOf
        [ closingBoundParser bound
            |> backtrackable
            |> map (always BoundFound)

        -- Without end here, chompIf would always succeed!
        , end
            |> map (always BoundNotFound)

        -- Chomp any by default.
        , succeed identity
            |. chompIf (always True)
            |= lazy (\_ -> chompUntilClosingBound bound)
        ]
        |> andThen
            (\boundFound ->
                case boundFound of
                    BoundFound ->
                        commit boundFound

                    BoundNotFound ->
                        problem <| "Could not find a closing bound for " ++ bound
            )


{-| Parser for matching closing bound!

    Closing bound should no be preceeded with a whitespace, and after the bound
    there should be no immediate special characters.

-}
closingBoundParser : String -> Parser ()
closingBoundParser bound =
    let
        charIsInBound : Char -> Bool
        charIsInBound c =
            bound
                |> String.toList
                |> List.member c

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
            [ chompIf charIsInBound
                |> backtrackable
                |> map (always True)
            , succeed False
            ]
        |> andThen isValidSymbol


{-| Special characters have special meanings, they can define context bounds.
Context here is bold, italic and strikethrough.
-}
isSpecialChar : Char -> Bool
isSpecialChar c =
    List.member c [ '*', '_', '~' ]


chompWhileNotSpecialChar : Parser ()
chompWhileNotSpecialChar =
    oneOf [ end, chompWhile (not << isSpecialChar) ]


{-| Parser which looks ahead and expects bounds. If a bound is not found,
chomp a character and check ahead again, until you find a bound, or end is
reached!
-}
chompWhileNotBound : Parser ()
chompWhileNotBound =
    oneOf
        [ boundsList
            |> List.map (symbol << boundToString)
            |> oneOf
            |> backtrackable
            |> map (always True)
        , end
        , chompIf (always True)
            |. lazy (\_ -> chompWhileNotBound)
        ]
        |> andThen commit


{-| Because of how inline content parsing works, basically stopping at any
special character and checking if it defines bold/italic/strikethrough context,
we can have a situation where the bounds are not valid, but the result of the
parsing is multiple consecutive Text nodes.

Eg. parsing string "This \*\* is \*\* non \*\* valid", produces

    [ Text "This ", Text "** is ", Text "** non ", Text "** valid" ]

This function will take all those consecutive Text's and merge them into one,
to give us this:

    [ Text "This ** is ** non ** valid" ]

-}
mergeConsecutiveText : InlineContent -> InlineContent -> InlineContent
mergeConsecutiveText res content =
    case content of
        (Text a) :: (Text b) :: other ->
            mergeConsecutiveText res (Text (a ++ b) :: other)

        first :: other ->
            mergeConsecutiveText (first :: res) other

        [] ->
            List.reverse res

module Markdown.Parsers.Heading exposing
    ( headingParser
    , headingUnderlineParser
    )

import Char.Parsers exposing (..)
import Markdown.Parsers.TextLine exposing (textLineParser)
import Markdown.Types exposing (..)
import Parser exposing (..)


{-| If we start chomping on some "#", it will be parsed as a header!
-}
headingParser : Parser MarkdownBlock
headingParser =
    Parser.succeed Heading
        |= headingTypeParser
        |. spaces
        |= textLineParser


headingTypeParser : Parser HeadingType
headingTypeParser =
    let
        toHeadingType : String -> () -> Maybe HeadingType
        toHeadingType hashes _ =
            hashesToHeadingType hashes

        toHeadingTypeParser : Maybe HeadingType -> Parser HeadingType
        toHeadingTypeParser =
            Maybe.map succeed >> Maybe.withDefault (problem "Not a heading!")
    in
    -- go down this parsing route if the first char is '#'
    chompIf ((==) '#')
        |. chompWhile ((==) '#')
        |> mapChompedString toHeadingType
        |> andThen toHeadingTypeParser



-- UNDERLINED HEADER LOOKAHEAD PARSERS


{-| Try to apply one of the two available parsers to the next line. If we are
successful in parsing a line full of '=' or '-', this parser will return
HeadingUnderline lookahead value, and if it fails it will be NoLookahead.
-}
headingUnderlineParser : Parser LookaheadContent
headingUnderlineParser =
    oneOf
        [ equalsUnderlinedHeadingParser
        , dashesUnderlinedHeadingParser
        ]


{-| This parser should succeed if there's at least one '=' to chomp.
-}
equalsUnderlinedHeadingParser : Parser LookaheadContent
equalsUnderlinedHeadingParser =
    chompOneOrMoreEquals
        |> mapChompedString (mapToUnderlinedHeading H1)


{-| This parser should succeed if tehre's at least one '-' to chomp.
-}
dashesUnderlinedHeadingParser : Parser LookaheadContent
dashesUnderlinedHeadingParser =
    chompOneOrMoreDashes
        |> mapChompedString (mapToUnderlinedHeading H2)


{-| We shouldn't get the case where this fn is called with an empty chomped
string, but just in case, we make sure there's something there.
-}
mapToUnderlinedHeading : HeadingType -> String -> () -> LookaheadContent
mapToUnderlinedHeading headingType chomped _ =
    if String.length chomped > 0 then
        HeadingUnderline headingType chomped

    else
        NoLookahead

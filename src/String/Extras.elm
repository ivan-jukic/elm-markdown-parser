module String.Extras exposing (..)

import Char.Extras as Char


startsOrEndsWithWhitespace : String -> Bool
startsOrEndsWithWhitespace str =
    startsWithWhitespace str || endsWithWhitespace str


startsWithWhitespace : String -> Bool
startsWithWhitespace str =
    case String.toList str of
        [] ->
            False

        start :: _ ->
            Char.isWhitespace start


endsWithWhitespace : String -> Bool
endsWithWhitespace str =
    case String.toList (String.reverse str) of
        [] ->
            False

        end :: _ ->
            Char.isWhitespace end

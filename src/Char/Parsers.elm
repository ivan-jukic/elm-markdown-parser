module Char.Parsers exposing (..)

import Char.Extras exposing (..)
import Parser exposing (..)


chompOneOrMoreEquals : Parser ()
chompOneOrMoreEquals =
    chompIf isEquals |. chompWhile isEquals


chompOneOrMoreDashes : Parser ()
chompOneOrMoreDashes =
    chompIf isDash |. chompWhile isDash


chompOnlyOneNewLine : Parser ()
chompOnlyOneNewLine =
    chompIf isNewLine


chompIfSpaceOrTab : Parser ()
chompIfSpaceOrTab =
    chompIf (\c -> c == ' ' || c == '\t')

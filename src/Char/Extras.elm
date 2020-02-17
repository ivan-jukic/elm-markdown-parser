module Char.Extras exposing (..)


isAlphaNumOrSpace : Char -> Bool
isAlphaNumOrSpace c =
    Char.isAlphaNum c || c == ' '


isWhitespace : Char -> Bool
isWhitespace c =
    c == ' ' || c == '\t' || c == '\n'


isNewLine : Char -> Bool
isNewLine =
    (==) '\n'


isNotNewLine : Char -> Bool
isNotNewLine =
    (/=) '\n'


isDash : Char -> Bool
isDash =
    (==) '-'


isEquals : Char -> Bool
isEquals =
    (==) '='

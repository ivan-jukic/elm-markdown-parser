module Char.Extras exposing (..)


isAlphaNumOrSpace : Char -> Bool
isAlphaNumOrSpace c =
    Char.isAlphaNum c || c == ' '


isWhitespace : Char -> Bool
isWhitespace c =
    c == ' ' || c == '\t' || c == '\n'


isNotWhitespace : Char -> Bool
isNotWhitespace =
    not << isWhitespace


isSpace : Char -> Bool
isSpace =
    (==) ' '


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

module Char.Extras exposing (..)


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

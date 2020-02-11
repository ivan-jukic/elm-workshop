module Char.Parsers exposing (..)

import Char.Extras exposing (..)
import Parser exposing (..)


chompOneOrMoreEquals : Parser ()
chompOneOrMoreEquals =
    succeed ()
        |. chompIf isEquals
        |. chompWhile isEquals


chompOneOrMoreDashes : Parser ()
chompOneOrMoreDashes =
    succeed ()
        |. chompIf isDash
        |. chompWhile isDash


chompOnlyOneNewLine : Parser ()
chompOnlyOneNewLine =
    succeed ()
        |. chompIf isNewLine

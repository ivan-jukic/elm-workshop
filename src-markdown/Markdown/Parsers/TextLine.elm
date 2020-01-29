module Markdown.Parsers.TextLine exposing (textLineParser)

import Parser exposing (..)


textLineParser : Parser String
textLineParser =
    succeed String.slice
        |= getOffset
        |. chompUntilEndOr "\n"
        |= getOffset
        |= getSource

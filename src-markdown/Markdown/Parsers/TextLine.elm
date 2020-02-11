module Markdown.Parsers.TextLine exposing (newLineParser, textLineParser)

import Char.Extras exposing (isNewLine, isNotNewLine)
import Markdown.Types exposing (MarkdownBlock(..))
import Parser exposing (..)


textLineParser : Parser String
textLineParser =
    succeed String.slice
        |= getOffset
        -- chomp only if not empty line
        |. chompIf isNotNewLine
        -- chomp whole line, or until end
        |. chompUntilEndOr "\n"
        |= getOffset
        |= getSource


newLineParser : Parser MarkdownBlock
newLineParser =
    succeed NewLine
        |. chompIf isNewLine

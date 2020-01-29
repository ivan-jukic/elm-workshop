module Markdown.Parsers.Body exposing (bodyParser)

import Parser exposing (..)
import Markdown.Types exposing (..)
import Markdown.Parsers.TextLine exposing (textLineParser)


bodyParser : Parser MarkdownBlock
bodyParser =
    let
        isNewLine : String -> MarkdownBlock
        isNewLine val =
            case val of
                "" ->
                    NewLine
                
                other ->
                    Body other
    in
    Parser.succeed isNewLine
        |= textLineParser
        |. spaces

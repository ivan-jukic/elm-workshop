module Markdown.Parsers.BlockContent exposing (blockContentParser)

import Markdown.Types exposing (..)
import Parser exposing (..)


blockContentParser : Parser BlockContent
blockContentParser =
    succeed <| Text "block parser"

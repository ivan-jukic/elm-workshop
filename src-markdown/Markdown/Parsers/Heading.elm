module Markdown.Parsers.Heading exposing (headingParser)

import Markdown.Parsers.TextLine exposing (textLineParser)
import Markdown.Types exposing (..)
import Parser exposing (..)


headingParser : Parser MarkdownBlock
headingParser =
    Parser.succeed Heading
        |= headingTypeParser
        |. spaces
        |= textLineParser


headingTypeParser : Parser HeadingType
headingTypeParser =
    let
        toHeadingType : String -> () -> Maybe HeadingType
        toHeadingType hashes _ =
            hashesToHeadingType hashes

        toHeadingTypeParser : Maybe HeadingType -> Parser HeadingType
        toHeadingTypeParser =
            Maybe.map succeed >> Maybe.withDefault (problem "Not a heading!")
    in
    chompWhile ((==) '#')
        |> mapChompedString toHeadingType
        |> andThen toHeadingTypeParser

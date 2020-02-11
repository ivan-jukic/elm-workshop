module Markdown.Parsers.Heading exposing
    ( headingParser
    , headingUnderlineParser
    )

import Char.Parsers exposing (..)
import Markdown.Parsers.TextLine exposing (textLineParser)
import Markdown.Types exposing (..)
import Parser exposing (..)


{-| If we start chomping on some "#", it will be parsed as a header!
-}
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
    -- go down this parsing route if the first char is '#'
    chompIf ((==) '#')
        |. chompWhile ((==) '#')
        |> mapChompedString toHeadingType
        |> andThen toHeadingTypeParser



-- UNDERLINED HEADER LOOKAHEAD PARSERS


headingUnderlineParser : Parser LookaheadContent
headingUnderlineParser =
    oneOf
        [ equalsUnderlinedHeadingParser
        , dashesUnderlinedHeadingParser
        ]


equalsUnderlinedHeadingParser : Parser LookaheadContent
equalsUnderlinedHeadingParser =
    chompOneOrMoreEquals
        |> mapChompedString (mapToUnderlinedHeading H1)


dashesUnderlinedHeadingParser : Parser LookaheadContent
dashesUnderlinedHeadingParser =
    chompOneOrMoreDashes
        |> mapChompedString (mapToUnderlinedHeading H2)


mapToUnderlinedHeading : HeadingType -> String -> () -> LookaheadContent
mapToUnderlinedHeading headingType chomped _ =
    if String.length chomped > 0 then
        HeadingUnderline headingType chomped

    else
        NoLookahead

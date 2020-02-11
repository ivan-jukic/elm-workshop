module Markdown.Types exposing (..)

{-| The final result of parsing will be a list of Markdown blocks!
-}


type alias MarkdownAst =
    List MarkdownBlock


{-| Each Markdown block represents a certain block thing in Markdown. For
example, headings, code blocks, block quotes, lists; but it does not include
other inline styles which may be present like bold, italic, inline code, etc.

TODO UnorderedList, OrderedList, Blockquote, Code, Rulers

-}
type MarkdownBlock
    = Heading HeadingType String
    | Body String
      -- New lines will be parsed, but filtered from the final result of parsing!
    | NewLine



-- MARKDOWN BLOCK CONTENTS


{-| Certain block elements might contain additional content that will need to
be parsed, and that can be nested

TODO add Italic, Strikethrough, Code, Sup, Sub, Link, Image, Emoji (?)

-}
type BlockContent
    = Bold BlockContent
    | Text String



-- LOOKBEHIND CONTENT


{-| Certain things can only be parsed if we know what comes after. For example,
we can have Heading 1 or Heading 2 defined by a new line of "=" or "-" of the
same length as the previous line with text.

    Eg.
        This is some H1 text
        ====================

        And this is H2 text
        -------------------

To parse this kind of content, we take a line, and then check if the next line
matches anything that would indicate that the previous line is special kind of
content. In our case line with "=" or "-" indicates that the previous line is
actually a heading.

-}
type LookaheadContent
    = HeadingUnderline HeadingType String
    | NoLookahead


lookaheadToHeading : String -> String -> HeadingType -> List MarkdownBlock
lookaheadToHeading headingText headingUnderline headingType =
    let
        areEqualLengths : String -> String -> Bool
        areEqualLengths a b =
            String.length a == String.length b
    in
    if areEqualLengths headingText headingUnderline then
        List.singleton <|
            Heading headingType headingText

    else
        -- We don't want to lose underline text, as it will be rendered as a
        -- regular body paragraph. This should give visual indication that the
        -- underline did not fit the title. They are in reversed order because
        -- once the parsing of the whole thing is done, parsed content order is
        -- reversed.
        [ Body headingUnderline
        , Body headingText
        ]



-- HEADING TYPE


{-| Determines heading types, depends on the number of `#` found during parsing.
-}
type HeadingType
    = H1
    | H2
    | H3
    | H4
    | H5
    | H6


{-| Function which will take the number of hashes that the parser has found and
returns what heading type that represents. If the string does not contain
and hashes, or there's more than 6, no heading type will be returned.
-}
hashesToHeadingType : String -> Maybe HeadingType
hashesToHeadingType hash =
    case hash of
        "#" ->
            Just H1

        "##" ->
            Just H2

        "###" ->
            Just H3

        "####" ->
            Just H4

        "#####" ->
            Just H5

        "######" ->
            Just H6

        _ ->
            Nothing

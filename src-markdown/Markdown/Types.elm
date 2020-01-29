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

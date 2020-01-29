module Markdown exposing (runParser)

import Markdown.Parsers.Body exposing (bodyParser)
import Markdown.Parsers.Heading exposing (headingParser)
import Markdown.Types exposing (..)
import Parser exposing (..)


runParser : String -> Result (List DeadEnd) MarkdownAst
runParser =
    Parser.run markdownParser


{-| Main markdown parser definition. It runs the parser in a loop, until there
is nothing left to parse, then it filters out any new lines that might have
been found, and keeps only relevant items.
-}
markdownParser : Parser MarkdownAst
markdownParser =
    markdownBlockParser
        |> loop []
        -- filter out new lines
        |> map (List.filter ((/=) NewLine))


{-| Parses one block of markdown. One block assumes that one of the parsers
will be successfull, and that the end of the file is not reached.

You may notice that we're testing for the end of the content before the body
parser. That is because the parser used in the bodyParser will return success
even if there's nothing to parse, and is also parser responsible for detecting
new lines. This is why we test for end, and if it's not the end, and all of the
previously tried parsers haven't produced any results, the next block will be
treated as just body text.

-}
markdownBlockParser : MarkdownAst -> Parser (Step MarkdownAst MarkdownAst)
markdownBlockParser result =
    let
        loop_ : Parser (MarkdownBlock -> Step MarkdownAst MarkdownAst)
        loop_ =
            succeed (\stmt -> Loop (stmt :: result))

        done_ : Parser (Step MarkdownAst MarkdownAst)
        done_ =
            succeed <| Done (List.reverse result)
    in
    Parser.oneOf
        [ loop_ |= headingParser
        , done_ |. end
        , loop_ |= bodyParser
        ]

module Markdown exposing (runParser)

import Char.Parsers exposing (..)
import Markdown.Parsers.Heading exposing (headingParser, headingUnderlineParser)
import Markdown.Parsers.TextLine exposing (newLineParser, textLineParser)
import Markdown.Types exposing (..)
import Parser exposing (..)


runParser : String -> Result (List DeadEnd) MarkdownAst
runParser =
    String.trim >> Parser.run markdownParser


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

        loopList_ : Parser (List MarkdownBlock -> Step MarkdownAst MarkdownAst)
        loopList_ =
            succeed (\stmtList -> Loop (List.append stmtList result))

        done_ : Parser (Step MarkdownAst MarkdownAst)
        done_ =
            succeed <| Done (List.reverse result)
    in
    Parser.oneOf
        -- If done succeeds first, it means that we've reached the end of the
        -- string we are parsing! Otherwise, try the other parsers.
        [ done_ |. end
        , loop_ |= headingParser
        , loopList_ |= bodyAndLookaheadParser
        , loop_ |= newLineParser
        ]


bodyAndLookaheadParser : Parser (List MarkdownBlock)
bodyAndLookaheadParser =
    Parser.succeed Tuple.pair
        |= textLineParser
        |= oneOf
            -- This is important here!!!!
            -- We are chomping one new line, and then checking what is coming
            -- after this new line; this is what our lookahead is. If there is
            -- no new line, chompOnlyOneNewLine will fail, and we will move on
            -- to the next parser, which is, in this case, the default succeed
            -- parser. If we didn't had that succeed parser, the whole thing
            -- would fail with a problem, as there was no other successful
            -- parsing route it could take.
            [ succeed identity
                |. chompOnlyOneNewLine
                |= oneOf
                    [ headingUnderlineParser

                    -- TODO add any other lookahead parsers here!
                    --
                    -- By default succeed with NoLookahead, otherwise the
                    -- parsing might fail, and we do want it not to.
                    , succeed NoLookahead
                    ]
            , succeed NoLookahead
            ]
        |> andThen lookaheadToMarkdownBlock


{-| Function which receives the parsed/chomped line and whatever the lookahead
content is. Lookahead content can only be one of the defined contents, if it's
anything else, chomping will not start and we get NoLookahed.
-}
lookaheadToMarkdownBlock : ( String, LookaheadContent ) -> Parser (List MarkdownBlock)
lookaheadToMarkdownBlock ( textLine, lookaheadLine ) =
    succeed <|
        case lookaheadLine of
            HeadingUnderline headingType underline ->
                -- Returns either heading markdown, or body if the text and
                -- underline lengths do not match (they must)
                lookaheadToHeading textLine underline headingType

            NoLookahead ->
                -- If the lookahead cannot be applied, treat any chomped text
                -- as body (later rendered as a regular paragraph)
                List.singleton (Body textLine)

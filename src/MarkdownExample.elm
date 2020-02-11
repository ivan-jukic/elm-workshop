module MarkdownExample exposing (..)

import Browser exposing (sandbox)
import Html exposing (Html, text)
import Markdown
import Markdown.Types exposing (..)
import Parser exposing (deadEndsToString)


testString : String
testString =
    """
### This is h3 heading with **bold** text

And this is just some new line with `code` and _italics_!

###### Here some more, and h6 heading


Heading with the weird syntax
-----------------------------


# H1 heading

and some regular text for the end without new line at the end"""


main : Program () Int msg
main =
    sandbox
        { init = 0
        , view = view
        , update = always identity
        }


view : Int -> Html msg
view _ =
    case Debug.log "md" <| Markdown.runParser testString of
        Ok md ->
            Html.div [] <|
                List.map
                    (\mdVal ->
                        case mdVal of
                            Heading H1 val ->
                                Html.h1 [] [ text val ]

                            Heading H2 val ->
                                Html.h2 [] [ text val ]

                            Heading H3 val ->
                                Html.h3 [] [ text val ]

                            Heading H4 val ->
                                Html.h4 [] [ text val ]

                            Heading H5 val ->
                                Html.h5 [] [ text val ]

                            Heading H6 val ->
                                Html.h6 [] [ text val ]

                            Body val ->
                                Html.p [] [ text val ]

                            NewLine ->
                                text ""
                     -- ignore new lines, they were filtered out, but need to cover the case
                    )
                    md

        Err err ->
            text <| deadEndsToString err

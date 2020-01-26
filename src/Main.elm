module Main exposing (..)

import Browser exposing (sandbox)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


main : Program () Int Msg
main =
    sandbox
        { init = 0
        , view = view
        , update = update
        }


type Msg
    = Incr
    | Decr


update : Msg -> Int -> Int
update msg model =
    case msg of
        Incr ->
            model + 1

        Decr ->
            model - 1


view : Int -> Html Msg
view model =
    div []
        [ button [ onClick Incr ] [ text "+" ]
        , div [] [ text (String.fromInt model) ]
        , button [ onClick Decr ] [ text "-" ]
        ]

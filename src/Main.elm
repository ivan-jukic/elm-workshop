module Main exposing (..)

import Browser exposing (sandbox)
import Html exposing (Html, div)


main : Program () Model Msg
main =
    sandbox
        { init = init
        , view = view
        , update = update
        }


type alias ToDoItem =
    String


type alias Model =
    { todoItems : List ToDoItem
    , newTodoItem : String
    }


init : Model
init =
    { todoItems = []
    , newTodoItem = ""
    }


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html Msg
view model =
    div [] []

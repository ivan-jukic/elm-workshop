module Main exposing (..)

import Browser exposing (sandbox)
import Html exposing (Html, div, input)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput)


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
    | SetNewTodoItemName String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        SetNewTodoItemName newName ->
            { model | newTodoItem = newName }


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input
                [ type_ "text"
                , value model.newTodoItem
                , onInput SetNewTodoItemName
                ]
                []
            ]
        ]

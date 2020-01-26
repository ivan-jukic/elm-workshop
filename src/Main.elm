module Main exposing (..)

import Browser exposing (sandbox)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)


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
    | AddTodoItem
    | SetNewTodoItemName String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        AddTodoItem ->
            if String.length model.newTodoItem > 0 then
                { model
                    | todoItems = model.newTodoItem :: model.todoItems
                    , newTodoItem = ""
                }

            else
                model

        SetNewTodoItemName newName ->
            { model | newTodoItem = newName }


view : Model -> Html Msg
view model =
    div []
        [ div []
            (model.todoItems
                |> List.reverse
                |> List.map
                    (\todoItem ->
                        div [] [ text todoItem ]
                    )
            )
        , div []
            [ input
                [ type_ "text"
                , value model.newTodoItem
                , onInput SetNewTodoItemName
                ]
                []
            , button
                [ onClick AddTodoItem
                ]
                [ text "Add +" ]
            ]
        ]

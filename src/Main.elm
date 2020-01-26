module Main exposing (..)

import Browser exposing (sandbox)
import Html exposing (Html, button, div, h4, input, text)
import Html.Attributes exposing (style, type_, value)
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
    | RemoveTodoItem String
    | SetNewTodoItemName String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        RemoveTodoItem removeItem ->
            { model
                | todoItems =
                    model.todoItems
                        |> List.filter (\item -> item /= removeItem)
            }

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
    div
        [ style "width" "400px"
        , style "margin" "20px auto"
        , style "padding" "20px"
        , style "background-color" "#F2F2F2"
        , style "border-radius" "4px"
        ]
        [ h4
            [ style "margin" "0 0 20px"
            ]
            [ text "ToDo App Workshop"
            ]
        , div
            [ style "margin-bottom" "20px"
            ]
            (model.todoItems
                |> List.reverse
                |> List.map
                    (\todoItem ->
                        div
                            [ style "display" "flex"
                            , style "padding" "6px 0"
                            ]
                            [ text todoItem
                            , button
                                [ onClick (RemoveTodoItem todoItem)
                                , style "padding" "0"
                                , style "width" "18px"
                                , style "height" "18px"
                                , style "color" "white"
                                , style "border" "none"
                                , style "margin-left" "auto"
                                , style "border-radius" "50px"
                                , style "background-color" "#999"
                                ]
                                [ text "×" ]
                            ]
                    )
            )
        , div
            [ style "display" "flex"
            ]
            [ input
                [ type_ "text"
                , value model.newTodoItem
                , onInput SetNewTodoItemName
                , style "flex" "1"
                ]
                []
            , button
                [ onClick AddTodoItem
                , style "cursor" "pointer"
                ]
                [ text "Add +" ]
            ]
        ]

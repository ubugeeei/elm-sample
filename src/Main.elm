module Main exposing (..)

import Browser
import Html exposing (Html, button, div, form, input, li, span, text, ul)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
import Time exposing (now, toMillis, utc)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Todo =
    { title : String
    , completed : Bool
    }


type alias TodoCreateInput =
    { title : String
    }


type alias Model =
    { todos : List Todo
    , todoCreateInput : TodoCreateInput
    }


init : Model
init =
    { todos = []
    , todoCreateInput = { title = "" }
    }



-- UPDATE


type Msg
    = CreateTodo
    | DeleteTodo Int
    | HandleInput String


update : Msg -> Model -> Model
update msg model =
    case msg of
        CreateTodo ->
            { model | todos = createTodo model.todos model.todoCreateInput, todoCreateInput = { title = "" } }

        DeleteTodo index ->
            { model | todos = deleteTodo model.todos index }

        HandleInput title ->
            { model | todoCreateInput = { title = title } }


createTodo todos todoCreateInput =
    todos ++ [ { title = todoCreateInput.title, completed = False } ]


deleteTodo todos index =
    List.take index todos ++ List.drop (index + 1) todos



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ form []
            [ input [ value model.todoCreateInput.title, onInput HandleInput ] []
            , button [ type_ "button", onClick CreateTodo ] [ text "CreateTodo" ]
            ]
        , todosView model.todos
        ]


todosView : List Todo -> Html Msg
todosView todos =
    ul [] (List.indexedMap todoView todos)


todoView : Int -> Todo -> Html Msg
todoView index todo =
    li []
        [ input [ type_ "checkbox" ] []
        , span []
            [ text todo.title
            , button [ type_ "button", onClick (DeleteTodo index) ] [ text "Delete" ]
            ]
        ]

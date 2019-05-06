port module Main exposing (Model, Msg(..), emptyModel, init, main, setStorage, update, view)

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json exposing (Decoder, field, string)
import Task



---- PROGRAM ----


main : Program (Maybe Model) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        }


port setStorage : Model -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , Cmd.batch [ setStorage newModel, cmds ]
    )



-- MODEL


type alias Model =
    { newTodo : String
    , todoList : List String
    }


emptyModel : Model
emptyModel =
    { newTodo = ""
    , todoList = []
    }


init : Maybe Model -> ( Model, Cmd Msg )
init flags =
    ( Maybe.withDefault emptyModel flags
    , Cmd.none
    )



-- UPDATE


type Msg
    = Change String
    | Add
    | Delete Int



--update : Msg -> Model -> Model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        isSpace =
            String.trim >> String.isEmpty
    in
    case msg of
        Change str ->
            ( { model | newTodo = str }
            , Cmd.none
            )

        Add ->
            if isSpace model.newTodo then
                ( model, Cmd.none )

            else
                ( { model
                    | todoList = model.newTodo :: model.todoList
                    , newTodo = ""
                  }
                , Cmd.none
                )

        Delete n ->
            let
                t =
                    model.todoList
            in
            ( { model
                | todoList = List.take n t ++ List.drop (n + 1) t
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ section [ class "hero is-primary" ]
            [ div [ class "hero-body" ]
                [ div [ class "container" ]
                    [ h1 [ class "title" ]
                        [ text "Elm Todo localStorage" ]
                    ]
                ]
            ]
        , section [ class "section" ]
            [ div [ class "container" ]
                [ section []
                    [ figure [ class "image container is-128x128" ]
                        [ img [ src "./logo.svg" ] []
                        ]
                    ]
                ]
            ]
        , section [ class "section" ]
            [ div [ class "container" ]
                [ div [ class "field has-addons" ]
                    [ div [ class "control" ]
                        [ input [ class "input", type_ "text", placeholder "input your todo", onInput Change, value model.newTodo ] []
                        ]
                    , div [ class "control" ]
                        [ a [ class "button is-info", onClick Add ] [ text "add todo" ]
                        ]
                    ]
                , ul [ class "list is-hoverable" ]
                    (showList model.todoList)
                ]
            ]
        , footer [ class "footer" ]
            [ div [ class "content has-text-centered" ]
                [ p []
                    [ a [ href "http://i-doctor.sakura.ne.jp/font/?p=37627" ] [ text "WordPressでフリーオリジナルフォント2" ]
                    ]
                ]
            ]
        ]


showList : List String -> List (Html Msg)
showList =
    let
        todos =
            List.indexedMap Tuple.pair

        column ( n, s ) =
            li [ class "list-item" ]
                [ div []
                    [ text s
                    , a [ class "button is-danger", onClick (Delete n) ] [ text "delete" ]
                    ]
                ]
    in
    todos >> List.map column

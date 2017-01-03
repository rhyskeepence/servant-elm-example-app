module Main exposing (..)

import Html exposing (div, button, text, input, p, h1, h2, form, program)
import Html.Attributes exposing (placeholder, value, type_, class)
import Html.Events exposing (onClick)
import String
import Json.Decode as Json
import Events exposing (onChange, onEnter, onSubmitPreventDefault)
import Generated.Api exposing (..)
import Task
import Http
import Result

main =
  Html.program
      { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }


type alias Model =
  { books : List Book
  , newBookTitle : String
  , newBookAuthorName : String
  , newBookAuthorYearOfBirth : Int
  }


init : ( Model, Cmd Action )
init =
  fetchBooks initModel


initModel : Model
initModel =
  { books = []
  , newBookTitle = ""
  , newBookAuthorName = ""
  , newBookAuthorYearOfBirth = 0
  }


type Action
  = FetchBooks
  | SetBooks (Result Http.Error (List Book))
  | SetNewBookTitle String
  | SetNewBookAuthorName String
  | SetNewBookAuthorYearOfBirth Int
  | CreateBook
  | CreatedBook (Result Http.Error (Book))


update : Action -> Model -> ( Model, Cmd Action )
update action model =
  case action of
    FetchBooks ->
      fetchBooks model

    SetBooks mNewBooks ->
      pure { model | books = Result.withDefault model.books mNewBooks }

    SetNewBookTitle title ->
      pure { model | newBookTitle = title }

    SetNewBookAuthorName name ->
      pure { model | newBookAuthorName = name }

    SetNewBookAuthorYearOfBirth year ->
      pure { model | newBookAuthorYearOfBirth = year }

    CreateBook ->
      if validate model then
        ( { model | newBookTitle = "", newBookAuthorName = "", newBookAuthorYearOfBirth = 0 }
        , postBooks
            { bookId = Nothing
            , title = model.newBookTitle
            , author =
                { name = model.newBookAuthorName
                , yearOfBirth = model.newBookAuthorYearOfBirth
                }
            }
            |> Http.send CreatedBook
        )
      else
        pure model

    CreatedBook newBook ->
      pure { model | books = 
        case newBook of
          Ok value -> value :: model.books
          Err err -> model.books
      }

fetchBooks : Model -> ( Model, Cmd Action )
fetchBooks model =
  ( model
  , getBooks
      |> Http.send SetBooks
  )


validate : Model -> Bool
validate { newBookTitle, newBookAuthorName } =
  List.all (not << String.isEmpty) [ newBookTitle, newBookAuthorName ]


view : Model -> Html.Html Action
view model =
  div
    [ class "container-fluid" ]
    [ h1 [] [ text "Books" ]
    , viewBookForm model
    , viewBookList model
    ]


viewBookList model =
  div
    []
    [ h2 [] [ text "All books" ]
    , div
        [ class "row" ]
        (List.map viewBook model.books)
    , button
        [ onClick FetchBooks
        , class "btn btn-default"
        ]
        [ text "Refresh book list" ]
    ]


viewBookForm : Model -> Html.Html Action
viewBookForm model =
  div
    [ class "row" ]
    [ div
        [ class "col-lg-12" ]
        [ h2 [] [ text "Create a book" ]
        , form
            [ class "form-inline"
            , onSubmitPreventDefault CreateBook
            ]
            [ div
                [ class "form-group" ]
                [ input
                    [ placeholder "Title"
                    , class "form-control"
                    , value model.newBookTitle
                    , onChange SetNewBookTitle
                    ]
                    []
                ]
            , div
                [ class "form-group" ]
                [ input
                    [ placeholder "Author"
                    , class "form-control"
                    , value model.newBookAuthorName
                    , onChange SetNewBookAuthorName
                    ]
                    []
                ]
            , div
                [ class "form-group" ]
                [ input
                    [ placeholder "Date of birth"
                    , class "form-control"
                    , value (toString model.newBookAuthorYearOfBirth)
                    , type_ "number"
                    , onChange (SetNewBookAuthorYearOfBirth << Maybe.withDefault 0 << Result.toMaybe << String.toInt)
                    ]
                    []
                ]
            , button
                [ type_ "submit"
                , class "btn btn-default"
                ]
                [ text "Create book" ]
            ]
        ]
    ]


viewBook : Book -> Html.Html msg
viewBook book =
  div
    [ class "col-lg-3" ]
    [ div
        [ class "panel panel-default" ]
        [ div
            [ class "panel-heading" ]
            [ text book.title ]
        , div
            [ class "panel-body" ]
            [ p
                []
                [ text
                    (book.author.name
                      ++ " (b."
                      ++ toString book.author.yearOfBirth
                      ++ ")"
                      ++ " {"
                      ++ Maybe.withDefault "unknown" book.bookId
                      ++ "}"
                    )
                ]
            ]
        ]
    ]


pure : a -> ( a, Cmd b )
pure model =
  ( model, Cmd.none )

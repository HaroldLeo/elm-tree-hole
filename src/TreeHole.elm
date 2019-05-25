module TreeHole exposing (..)

-- Add/modify imports if you'd like. ---------------------------------

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode

-- Model -------------------------------------------------------------

main : Program Flags Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

type alias Post =
  { title : String
  , author : String
  , article : String
  , id : Int }

type alias Model =
  { numPost : Int
  , posts : List Post
  , newTitle : String
  , newAuthor : String
  , newArticle : String
  }

type alias Flags = ()

type Msg = NewPost (Result Http.Error Post)
         | Upload
         | GetPost (Result Http.Error (List Post))
         | NewTitle String
         | NewAuthor String
         | NewArticle String

init : Flags -> (Model, Cmd Msg)
init () =
  (initModel, getPosts)

initModel : Model
initModel =
  { numPost = 0
  , posts = []
  , newTitle = ""
  , newAuthor = ""
  , newArticle = ""}

-- Update ------------------------------------------------------------

postsURL : String
postsURL = "http://localhost:3000/posts"

getPosts : Cmd Msg
getPosts =
  (Decode.list postDecoder)
    |> Http.get postsURL
    |> Http.send GetPost

postDecoder : Decoder Post
postDecoder =
  Decode.map4 Post
    (field "title" Decode.string)
    (field "author" Decode.string)
    (field "article" Decode.string)
    (field "id" Decode.int)

encodePost : Model -> Encode.Value
encodePost model =
  Encode.object
    [ ("title", Encode.string model.newTitle)
    , ("author", Encode.string model.newAuthor)
    , ("article", Encode.string model.newArticle)
    ]

sendPost : Model -> Cmd Msg
sendPost model =
    let
      body =
          encodePost model
                |> Http.jsonBody
      request =
          Http.post postsURL body postDecoder
    in
      Http.send NewPost request

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Upload ->
      (model, sendPost model)

    NewPost (Ok score) ->
      ({ model | newTitle = ""
               , newAuthor = ""
               , newArticle = ""}, getPosts)

    NewPost (Err error) ->
      let
        _ = Debug.log "Cannot post a new post" error
      in
        (model, Cmd.none)

    GetPost (Ok lists) ->
      ({ model | posts = lists,
                 numPost = List.length(lists) }, Cmd.none)

    GetPost (Err error) ->
      let
        _ = Debug.log "cannoot request posts" error
      in
        (model, Cmd.none)

    NewTitle t ->
      ({ model | newTitle = t }, Cmd.none)

    NewAuthor a ->
      ({ model | newAuthor = a }, Cmd.none)

    NewArticle ar ->
      ({ model | newArticle = ar }, Cmd.none)


-- View --------------------------------------------------------------

viewHeader : String -> Html Msg
viewHeader title =
  header []
    [ h1 [] [ text title ] ]


viewFooter : Html Msg
viewFooter =
  footer []
    [ a
        [ href "https://github.com/HaroldLeo" ]
        [ text "Written by Hongji Liu" ]
    ]

viewPost : Post -> Html msg
viewPost post =
  article []
    [ h2 [ class "title" ] [ text post.title ]
    , h4 [ class "author" ] [ text post.author ]
    , p [ class "article" ] [ text post.article ]
    ]


viewPostList : List Post -> Html msg
viewPostList posts =
  let
    postList =
      List.map viewPost posts
  in
    ul [] postList

viewPostInput : Model -> Html Msg
viewPostInput model =
  div [ class "posting" ]
        [ input
          [ type_ "text"
          , placeholder "Title"
          , autofocus True
          , onInput NewTitle
          , value model.newTitle
          ]
          []
        , input
          [ type_ "text"
          , placeholder "Author"
          , autofocus True
          , onInput NewAuthor
          , value model.newAuthor
          ]
          []
        , textarea
          [ cols 40
          , rows 10
          , placeholder "Article"
          , autofocus True
          , onInput NewArticle
          , value model.newArticle
          ]
          []
        , button
          [ class "buttons"
          , onClick Upload
          ]
          [ text "Submit" ]
      ]

view : Model -> Html Msg
view model =
  div [ class "web" ]
      [ viewHeader "Tree Hole"
      , viewPostInput model
      , viewPostList model.posts
      , viewFooter
      ]

module TreeHole exposing (..)

-- Add/modify imports if you'd like. ------------------------------------------

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode
import Json.Decode.Pipeline as DecodePipeline

-- Main -----------------------------------------------------------------------

main : Program Flags Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

-- Model ----------------------------------------------------------------------

type PostState = Posting | None

type alias Comment =
  { postId : Int
  , author : String
  , content : String
  , id : Int
  }

type alias Post =
  { title : String
  , author : String
  , article : String
  , vote : Int
  , comments : List Comment
  , clicked : Bool
  , newCommentAuthor : String
  , newCommentContent : String
  , id : Int
  }

type alias Model =
  { posts : List Post
  , comments : List Comment
  , newTitle : String
  , newAuthor : String
  , newArticle : String
  , postState : PostState
  , newComPostId : Int
  }

type alias Flags = ()

init : Flags -> (Model, Cmd Msg)
init () =
  (initModel, getPosts)

initModel : Model
initModel =
  { posts = []
  , comments = []
  , newTitle = ""
  , newAuthor = ""
  , newArticle = ""
  , postState = None
  , newComPostId = 0
  }

-- Update ---------------------------------------------------------------------

type Msg = NewPost (Result Http.Error Post)
         | Upload
         | GetPost (Result Http.Error (List Post))
         | NewTitle String
         | NewAuthor String
         | NewArticle String
         | Upvote Int
         | Downvote Int
         | VoteUpdated (Result Http.Error Post)
         | ChangePostState PostState
         | ChangeCommentState Int Bool
         | GetComment (Result Http.Error (List Comment))
         | NewCommentAuthor String
         | NewCommentContent String
         | CommentUpdated (Result Http.Error Comment)
         | SendComment Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Upload ->
      (model, sendPost model)

    NewPost (Ok score) ->
      ({ model | newTitle = ""
               , newAuthor = ""
               , newArticle = ""
               , postState = None}, getPosts)

    NewPost (Err error) ->
      let
        _ = Debug.log "Cannot post a new post" error
      in
        (model, Cmd.none)

    GetPost (Ok lists) ->
      ({ model | posts = List.reverse (List.sortBy .id lists)}, Cmd.none)

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

    Upvote id ->
      case (findPostById id model.posts) of
        Just post -> (model, updatePost (upvote post))
        Nothing -> (model, Cmd.none)

    Downvote id ->
      case (findPostById id model.posts) of
        Just post -> (model, updatePost (downvote post))
        Nothing -> (model, Cmd.none)

    VoteUpdated _ ->
      (model, getPosts)

    ChangePostState state ->
      ({ model | postState = state}, Cmd.none)

    ChangeCommentState id state ->
      case state of
        True ->
          ({ model | posts = updateState model.posts id state
                   , newComPostId = id}, getComments)
        False ->
          ({ model | posts = updateState model.posts id state
                   , newComPostId = 0}, getComments)

    GetComment (Ok lists) ->
      ({ model | comments = List.reverse (List.sortBy .postId lists)
               , posts = distributor model.posts lists}, Cmd.none)

    GetComment (Err error) ->
      let
        _ = Debug.log "cannoot request comments" error
      in
        (model, Cmd.none)

    NewCommentAuthor author ->
      ({ model | posts = updateComAuthor model.posts model.newComPostId author }
      , Cmd.none)

    NewCommentContent content ->
      ({ model | posts =
                       updateComContent model.posts model.newComPostId content }
      , Cmd.none)

    CommentUpdated _ ->
      (model, getComments)

    SendComment id ->
      case (findPostById id model.posts) of
        Just post ->
          ({ model | posts = sendingCom model.posts model.newComPostId }
                   , sendComment post)
        Nothing -> (model, Cmd.none)


-- Posts

postsURL : String
postsURL = "http://localhost:3000/posts/"

getPosts : Cmd Msg
getPosts =
  (Decode.list postDecoder)
    |> Http.get postsURL
    |> Http.send GetPost

postDecoder : Decoder Post
postDecoder =
  Decode.succeed Post
    |> DecodePipeline.required "title" Decode.string
    |> DecodePipeline.required "author" Decode.string
    |> DecodePipeline.required "article" Decode.string
    |> DecodePipeline.required "vote" Decode.int
    |> DecodePipeline.hardcoded []
    |> DecodePipeline.hardcoded False
    |> DecodePipeline.hardcoded ""
    |> DecodePipeline.hardcoded ""
    |> DecodePipeline.required "id" Decode.int

encodePost : Model -> Encode.Value
encodePost model =
  Encode.object
    [ ("title", Encode.string model.newTitle)
    , ("author", Encode.string model.newAuthor)
    , ("article", Encode.string model.newArticle)
    , ("vote", Encode.int 0)
    ]

postEncoder : Post -> Encode.Value
postEncoder post =
  Encode.object
    [ ("title", Encode.string post.title)
    , ("author", Encode.string post.author)
    , ("article", Encode.string post.article)
    , ("vote", Encode.int post.vote)
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

-- Comments

commentsURL : String
commentsURL = "http://localhost:3000/comments"

getComments : Cmd Msg
getComments =
  (Decode.list commentDecoder)
    |> Http.get commentsURL
    |> Http.send GetComment

commentDecoder : Decoder Comment
commentDecoder =
  Decode.map4 Comment
    (field "postId" Decode.int)
    (field "author" Decode.string)
    (field "content" Decode.string)
    (field "id" Decode.int)

encodeComment : Post -> Encode.Value
encodeComment post =
  Encode.object
    [ ("postId", Encode.int post.id)
    , ("author", Encode.string post.newCommentAuthor)
    , ("content", Encode.string post.newCommentContent)
    ]

sendComment : Post -> Cmd Msg
sendComment post =
  let
    body =
        encodeComment post
              |> Http.jsonBody
    request =
        Http.post commentsURL body commentDecoder
  in
    Http.send CommentUpdated request

distributor : List Post -> List Comment -> List Post
distributor posts comments =
    case posts of
      [] -> []
      x :: xs ->
        { title = x.title
        , author = x.author
        , article = x.article
        , vote = x.vote
        , comments = List.filter (\c -> c.postId == x.id) comments
        , clicked = x.clicked
        , newCommentAuthor = x.newCommentAuthor
        , newCommentContent = x.newCommentContent
        , id = x.id } :: (distributor xs comments)

updateState : List Post -> Int -> Bool -> List Post
updateState posts id state =
  case posts of
    [] -> []
    x :: xs ->
      if x.id == id then
        { title = x.title
        , author = x.author
        , article = x.article
        , vote = x.vote
        , comments = x.comments
        , clicked = state
        , newCommentAuthor = x.newCommentAuthor
        , newCommentContent = x.newCommentContent
        , id = x.id } :: xs
      else x :: (updateState xs id state)

updateComAuthor : List Post -> Int -> String -> List Post
updateComAuthor posts id author =
  case posts of
    [] -> []
    x :: xs ->
      if x.id == id then
        { title = x.title
        , author = x.author
        , article = x.article
        , vote = x.vote
        , comments = x.comments
        , clicked = x.clicked
        , newCommentAuthor = author
        , newCommentContent = x.newCommentContent
        , id = x.id } :: xs
      else x :: (updateComAuthor xs id author)

updateComContent : List Post -> Int -> String -> List Post
updateComContent posts id content =
  case posts of
    [] -> []
    x :: xs ->
      if x.id == id then
        { title = x.title
        , author = x.author
        , article = x.article
        , vote = x.vote
        , comments = x.comments
        , clicked = x.clicked
        , newCommentAuthor = x.newCommentAuthor
        , newCommentContent = content
        , id = x.id } :: xs
      else x :: (updateComContent xs id content)

sendingCom : List Post -> Int -> List Post
sendingCom posts id =
  case posts of
    [] -> []
    x :: xs ->
      if x.id == id then
        { title = x.title
        , author = x.author
        , article = x.article
        , vote = x.vote
        , comments = x.comments
        , clicked = x.clicked
        , newCommentAuthor = ""
        , newCommentContent = ""
        , id = x.id } :: xs
      else x :: (sendingCom xs id)
-- Upvotes/Downvotes

findPostById : Int -> List Post -> Maybe Post
findPostById id posts =
  case posts
        |> List.filter (\post -> post.id == id)
        |> List.head of
    Just post -> Just post
    Nothing -> Nothing

upvote : Post -> Post
upvote post =
  { title = post.title
  , author = post.author
  , article = post.article
  , vote = post.vote + 1
  , comments = post.comments
  , clicked = post.clicked
  , newCommentAuthor = post.newCommentAuthor
  , newCommentContent = post.newCommentContent
  , id = post.id }

downvote : Post -> Post
downvote post =
  { title = post.title
  , author = post.author
  , article = post.article
  , vote = post.vote - 1
  , comments = post.comments
  , clicked = post.clicked
  , newCommentAuthor = post.newCommentAuthor
  , newCommentContent = post.newCommentContent
  , id = post.id }

updatePost : Post -> Cmd Msg
updatePost post =
    let
      body =
        postEncoder post
          |> Http.jsonBody

      request =
        Http.request
          { method = "PATCH"
          , headers = []
          , url = postsURL ++ (Debug.toString post.id)
          , body = body
          , expect = Http.expectJson postDecoder
          , timeout = Nothing
          , withCredentials = False
          }
    in
      Http.send VoteUpdated request

-- View -----------------------------------------------------------------------

viewHeader : String -> Html Msg
viewHeader title =
  header [ class "header" ]
    [ h1 [] [ text title ]
    , h4 [] [ text "...where you can put your secrets in *shh*" ]
    ]


viewFooter : Html Msg
viewFooter =
  div []
      [ footer []
        [ a
          [ href "https://github.com/HaroldLeo" ]
          [ text "Written by Hongji Liu" ]
        ]
      ]

viewArticle : List String -> List (Html Msg)
viewArticle list =
  case list of
    [] -> []
    first :: rest ->
      (p [ class "article" ] [ text first ]) :: viewArticle rest

viewVote : Post -> List (Html Msg)
viewVote post =
  [ div [ class "vote" ]
  [ button [ class "upvote", onClick (Upvote post.id)]
           [ text "⇧" ]
  , h5 [ class "vote-num" ]
       [ text (String.fromInt post.vote) ]
  , button [ class "downvote", onClick (Downvote post.id)]
           [ text "⇩" ]
  ]]

viewPost : Post -> Html Msg
viewPost post =
  article [ class "post" ]
    ([ h2 [ class "title" ] [ text post.title ]
     , h4 [ class "author" ] [ text post.author ]
     ] ++ viewArticle (String.split "\n" post.article)
     ++ viewVote post ++ viewCommentList post)

viewPostList : List Post -> Html Msg
viewPostList posts =
  let
    postList = List.map viewPost posts
  in
    ul [ class "posts" ] postList

viewComment : Comment -> Html Msg
viewComment comment =
    article [ class "comment" ]
      ([ h5 [ class "author" ] [ text (comment.author ++ ":") ]]
      ++ viewArticle (String.split "\n" comment.content))

viewCommentList : Post -> List (Html Msg)
viewCommentList post =
    let
      commentList = List.map viewComment post.comments
    in
    case post.clicked of
      True ->
        [ div [ class "commenting" ]
              [ button
                [ class "comment-button"
                , onClick (ChangeCommentState post.id False)
                ]
                [ text "comments" ]
              ]
        , div [ class "commenting" ]
              [ input
                [ type_ "text"
                , placeholder "you, the name"
                , autofocus True
                , onInput NewCommentAuthor
                , value post.newCommentAuthor
                ]
                []
              ]
        , div [ class "commenting" ]
              [ textarea
                [ cols 10
                , rows 5
                , placeholder "any thoughts?"
                , autofocus True
                , onInput NewCommentContent
                , value post.newCommentContent
                ]
                []
              ]
        , div [ class "commenting" ]
              [ button
                [ class "comment-submit"
                , onClick (SendComment post.id)
                , disabled (emptyComment post)
                ]
                [ text "don't regret" ]
              ]
            ] ++ [ ul [ class "comments" ] commentList ]
      False ->
        [div [ class "commenting"]
          [button
            [ class "comment-button"
            , onClick (ChangeCommentState post.id True)
            ]
            [ text "comments" ]
          ]
        ]

emptyComment : Post -> Bool
emptyComment post =
    if post.newCommentAuthor == "" || post.newCommentContent == ""
      then True
    else
      False

emptyPost : Model -> Bool
emptyPost model =
    if model.newTitle == "" || model.newAuthor == "" || model.newArticle == ""
      then True
    else
      False

viewPostInput : Model -> Html Msg
viewPostInput model =
  case model.postState of
    Posting ->
      div [ class "posting" ]
          [ div [ class "form-group" ]
                [ button
                  [ class "post-button"
                  , onClick (ChangePostState None)
                  ]
                  [ text "let go" ]
                ]
          , div [ class "form-group" ]
                [ input
                  [ type_ "text"
                  , placeholder "a bit of a summary"
                  , autofocus True
                  , onInput NewTitle
                  , value model.newTitle
                  , class "post-title"
                  ]
                  []
                ]
          , div [ class "form-group" ]
                [ input
                  [ type_ "text"
                  , placeholder "who are you"
                  , autofocus True
                  , onInput NewAuthor
                  , value model.newAuthor
                  , class "post-author"
                  ]
                  []
                ]
          , div [ class "form-group" ]
                [ textarea
                  [ cols 50
                  , rows 10
                  , placeholder "please elaborate"
                  , autofocus True
                  , onInput NewArticle
                  , value model.newArticle
                  , class "post-article"
                  ]
                  []
                ]
          , div [ class "form-group" ]
                [ button
                  [ class "submit-button"
                  , onClick Upload
                  , disabled (emptyPost model)
                  ]
                  [ text "push it in" ]
                ]
          ]
    None ->
      div [ class "posting" ]
          [ button
            [ class "post-button"
            , onClick (ChangePostState Posting)
            ]
            [ text "a new post" ]
          ]

view : Model -> Html Msg
view model =
  div [ id "container" ]
      [ viewHeader "a giant hole of a mysterious tree"
      , viewPostInput model
      , viewPostList model.posts
      , viewFooter
      ]

module Picshare exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, disabled, placeholder, src, type_, value)
import Html.Events exposing (onClick, onSubmit)
import Html.Events.Extra exposing (onChange)


baseUrl : String
baseUrl =
    "https://programming-elm.com/"


type alias Model =
    { url : String
    , caption : String
    , liked : Bool
    , comments : List String
    , newComment : String
    }


init : Model
init =
    { url = baseUrl ++ "1.jpg"
    , caption = "Surfing"
    , liked = False
    , comments = [ "Cowabunga, dude!" ]
    , newComment = ""
    }


type Msg
    = ToggleLiked
    | UpdateComment String
    | SaveComment


view : Model -> Html Msg
view model =
    div []
        [ div [ class "header" ]
            [ h1 [] [ text "Picshare" ] ]
        , div [ class "content-flow" ]
            [ viewDetailedPhoto model ]
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleLiked ->
            { model | liked = not model.liked }

        UpdateComment comment ->
            { model | newComment = comment }

        SaveComment ->
            saveNewComment model


saveNewComment : Model -> Model
saveNewComment model =
    let
        comment =
            String.trim model.newComment
    in
    case comment of
        "" ->
            model

        _ ->
            { model
                | comments = model.comments ++ [ comment ]
                , newComment = ""
            }


viewDetailedPhoto : Model -> Html Msg
viewDetailedPhoto model =
    div [ class "detailed-photo" ]
        [ img [ src model.url ] []
        , div [ class "photo-info" ]
            [ viewLoveButton model
            , h2 [ class "caption" ] [ text model.caption ]
            , viewComments model
            ]
        ]


viewLoveButton : Model -> Html Msg
viewLoveButton model =
    let
        buttonClass =
            if model.liked then
                "fa-heart"

            else
                "fa-heart-o"
    in
    div [ class "like-button" ]
        [ i
            [ class "fa fa-2x"
            , class buttonClass
            , onClick ToggleLiked
            ]
            []
        ]


viewComment : String -> Html Msg
viewComment comment =
    li []
        [ strong [] [ text "Comment:" ]
        , text (" " ++ comment)
        ]


viewCommentList : List String -> Html Msg
viewCommentList comments =
    case comments of
        [] ->
            text ""

        _ ->
            div [ class "comments" ]
                [ ul []
                    (List.map viewComment comments)
                ]


viewComments : Model -> Html Msg
viewComments model =
    div []
        [ viewCommentList model.comments
        , form [ class "new-comment", onSubmit SaveComment ]
            [ input
                [ type_ "text"
                , placeholder "Add a comment ..."
                , value model.newComment
                , onChange UpdateComment
                ]
                []
            , button
                [ disabled (String.isEmpty model.newComment) ]
                [ text "Save" ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }

module Main exposing (init, main, update, view)

import Browser
import Browser.Navigation as Navigation
import Html exposing (Html, a, div, h1, img, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Model exposing (..)
import Url exposing (Url)
import Url.Parser as Parser


init : flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        data =
            parseAuthorizationUrl url

        accessToken =
            data |> Maybe.andThen (\d -> d.accessToken)
    in
    case accessToken of
        Just token ->
            ( { key = key, authData = data, profile = None }, getProfile token )

        Nothing ->
            ( { key = key, authData = data, profile = None }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SignInRequested ->
            ( model, performAuthorization )

        ChangedUrl urlRequest ->
            let
                _ =
                    Debug.log "Request" urlRequest
            in
            ( model, Cmd.none )

        ClickedLink _ ->
            ( model, Cmd.none )

        GotProfile request ->
            case request of
                Ok profile ->
                    ( { model | profile = Reddit profile }, Cmd.none )

                Err err ->
                    let
                        _ =
                            Debug.log "Error:" err
                    in
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


bodyView : Model -> Html Msg
bodyView model =
    div []
        [ a [ href "#", onClick SignInRequested ] [ text "Sign in" ]
        , div []
            [ case model.profile of
                Reddit profile ->
                    text profile.name

                None ->
                    text ""
            ]
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Social"
    , body = [ bodyView model ]
    }



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = ChangedUrl
        , onUrlChange = ClickedLink
        }

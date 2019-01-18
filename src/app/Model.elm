module Model exposing (AuthData, Model, Msg(..), Profile, Service(..), SignInRequest, authorizationData, getProfile, parseAuthorizationUrl, performAuthorization, profileDecoder, signInRequestData)

import Browser exposing (UrlRequest)
import Browser.Navigation as Navigation exposing (load)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import List.Extra exposing (find)
import Url exposing (Url)
import Url.Builder exposing (absolute, crossOrigin, string)
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as Query


type alias AuthData =
    { accessToken : Maybe String
    , expiresIn : Maybe Int
    }


type alias Model =
    { key : Navigation.Key, authData : Maybe AuthData, profile : Service Profile }


type Service a
    = None
    | Reddit a


type alias SignInRequest =
    { clientId : String
    , authorizationEndpoint : String
    , responseType : String
    , redirectUri : String
    , scope : List String
    }


type alias Profile =
    { id : String
    , name : String
    }


type Msg
    = SignInRequested
    | ChangedUrl UrlRequest
    | ClickedLink Url
    | GotProfile (Result Http.Error Profile)
    | NoOp


authorizationData : Query.Parser AuthData
authorizationData =
    Query.map2 AuthData
        (Query.string "access_token")
        (Query.int "expires_in")


parseAuthorizationUrl : Url -> Maybe AuthData
parseAuthorizationUrl url =
    Parser.parse (Parser.s "auth" <?> authorizationData) { url | query = url.fragment }


signInRequestData : SignInRequest
signInRequestData =
    { clientId = "oTw-FCQWW-SYyA"
    , authorizationEndpoint = "https://www.reddit.com/api/v1/authorize"
    , responseType = "token"
    , redirectUri = "http://192.168.1.159:3000/auth"
    , scope = [ "identity", "edit", "history", "mysubreddits", "wikiread" ]
    }


performAuthorization : Cmd Msg
performAuthorization =
    let
        authData =
            signInRequestData
    in
    load
        (crossOrigin
            authData.authorizationEndpoint
            []
            [ string "client_id" authData.clientId
            , string "response_type" authData.responseType
            , string "redirect_uri" authData.redirectUri
            , string "scope" (String.join " " authData.scope)
            , string "state" "state"
            ]
        )


profileDecoder : Decode.Decoder Profile
profileDecoder =
    Decode.succeed Profile
        |> required "id" Decode.string
        |> required "name" Decode.string


getProfile : String -> Cmd Msg
getProfile accessToken =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Authorization" ("bearer " ++ accessToken)
            ]
        , url = "https://oauth.reddit.com/api/v1/me"
        , body = Http.emptyBody
        , expect = Http.expectJson GotProfile profileDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

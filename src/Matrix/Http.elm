module Matrix.Http exposing
    ( get, post, request
    , Expect, expectJson, expectWhatever
    , Error(..)
    , urlDecoder
    )

{-| Send Matrix HTTP requests.

Matrix specifies how clients talk to homeservers. This module mirrors Elm's
`Http` API, but it builds Matrix URLs for you and understands Matrix error
responses.


## Requests

@docs get, post, request


## Expect

@docs Expect, expectJson, expectWhatever


## Errors

@docs Error


## Extra

@docs urlDecoder

-}

import Dict exposing (Dict)
import Duration exposing (Duration)
import Http
import Internal.Tools.DecodeExtra as DE
import Json.Decode as D
import Json.Encode as E
import Matrix.Types.AccessToken as AccessToken exposing (AccessToken)
import Matrix.Types.HomeserverUrl as HomeserverUrl exposing (HomeserverUrl)
import Url exposing (Url)


{-| A standard error to receive back from contacting the Matrix server.

  - `HttpError` means a standard Elm `Http.Error` occurred while sending the
    request.
  - `InvalidJSONResponse` means the Matrix server returned a response that
    isn't properly formatted according to the specification.
  - `Ratelimit` means the server received too many requests in a short period of
    time. If a `Duration` is included, that's the recommended time to wait before
    sending another request.
  - `ServerError` means the Matrix server received your request and returned an
    error in the way that Matrix would normally do. The `errorCode` is always
    there, but the human-readable `errorHuman` message is optional.

This error type is meant to help you draw a clean line between networking
problems and Matrix-specific failures.

-}
type Error
    = HttpError Http.Error
    | InvalidJSONResponse String
    | InvalidResponse String
    | Ratelimit (Maybe Duration)
    | ServerError
        { errorCode : String
        , errorHuman : Maybe String
        , keys : Dict String E.Value
        }


{-| Logic for interpreting a Matrix response body.

You normally create an `Expect` value with `expectJson` or `expectWhatever`.
It is kept opaque so you can only build it in safe ways.

-}
type Expect msg
    = Expect (Http.Expect msg)


{-| Decode a standard Matrix error body. The spec only requires the `errcode`
field, so the human-readable `error` message may be missing.
-}
errorDecoder : D.Decoder { errorCode : String, errorHuman : Maybe String, keys : Dict String E.Value }
errorDecoder =
    D.map3
        (\ec eh ks ->
            { errorCode = ec, errorHuman = eh, keys = ks }
        )
        (D.field "errcode" D.string)
        (DE.opField "error" D.string)
        (D.dict D.value)


{-| Expect the response body to be JSON.

    import Json.Decode as D
    import Matrix.Http as MatrixHttp

    type Msg
        = GotProfile (Result MatrixHttp.Error String)

    request : Cmd Msg
    request =
        MatrixHttp.get
            { accessToken = Nothing
            , baseUrl = homeserver
            , path = [ "_matrix", "client", "v3", "profile", "@alice:example.org", "displayname" ]
            , expect = MatrixHttp.expectJson GotProfile (D.field "displayname" D.string)
            }

If decoding fails, you get `InvalidJSONResponse` or `ServerError` depending on
the body the server returned.

-}
expectJson : (Result Error a -> msg) -> D.Decoder a -> Expect msg
expectJson toMsg decoder =
    Expect <| Http.expectStringResponse toMsg (fromHttpError decoder)


{-| Expect the response body to be whatever. It does not matter. Ignore it!

This is handy for endpoints that return `{}` or no meaningful content.

-}
expectWhatever : (Result Error () -> msg) -> Expect msg
expectWhatever toMsg =
    Expect <| Http.expectStringResponse toMsg (fromHttpError (D.succeed ()))


{-| Transform a Matrix server's response into JSON, or into an error type
if the information cannot be extracted.
-}
fromHttpError : D.Decoder a -> Http.Response String -> Result Error a
fromHttpError decoder response =
    case response of
        Http.BadUrl_ url ->
            Err <| HttpError <| Http.BadUrl url

        Http.Timeout_ ->
            Err <| HttpError <| Http.Timeout

        Http.NetworkError_ ->
            Err <| HttpError <| Http.NetworkError

        Http.BadStatus_ metadata body ->
            case D.decodeString errorDecoder body of
                Ok v ->
                    Err <| ServerError v

                Err _ ->
                    -- TODO: Ratelimit
                    -- TODO: Use Retry-After header
                    Err <| HttpError <| Http.BadStatus metadata.statusCode

        Http.GoodStatus_ _ body ->
            case D.decodeString decoder body of
                Ok v ->
                    Ok v

                Err e ->
                    case D.decodeString errorDecoder body of
                        Ok v ->
                            Err <| ServerError v

                        Err _ ->
                            Err <| HttpError <| Http.BadBody <| D.errorToString e


{-| Create a GET request.

    import Matrix.Http as MatrixHttp

    getProfile : Cmd Msg
    getProfile =
        MatrixHttp.get
            { accessToken = Just accessToken
            , baseUrl = homeserver
            , path = [ "_matrix", "client", "v3", "profile", "@alice:example.org", "displayname" ]
            , expect = MatrixHttp.expectWhatever GotDone
            }

This builds the URL for you and automatically adds the `Authorization` header
when you provide an access token.

-}
get :
    { accessToken : Maybe AccessToken
    , baseUrl : HomeserverUrl
    , path : List String
    , expect : Expect msg
    }
    -> Cmd msg
get data =
    request
        { method = "GET"
        , accessToken = data.accessToken
        , baseUrl = data.baseUrl
        , path = data.path
        , query = []
        , body = Nothing
        , expect = data.expect
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Create a POST request.

    import Json.Encode as E
    import Matrix.Http as MatrixHttp

    sendMessage : Cmd Msg
    sendMessage =
        MatrixHttp.post
            { accessToken = Just accessToken
            , baseUrl = homeserver
            , path = [ "_matrix", "client", "v3", "rooms", "!room:example.org", "send", "m.room.message", "123" ]
            , body = E.object [ ( "msgtype", E.string "m.text" ), ( "body", E.string "Hi!" ) ]
            , expect = MatrixHttp.expectWhatever GotDone
            }

-}
post :
    { accessToken : Maybe AccessToken
    , baseUrl : HomeserverUrl
    , path : List String
    , body : E.Value
    , expect : Expect msg
    }
    -> Cmd msg
post data =
    request
        { method = "POST"
        , accessToken = data.accessToken
        , baseUrl = data.baseUrl
        , path = data.path
        , query = []
        , body = Just data.body
        , expect = data.expect
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Create a custom request.

Use this when you need query parameters, a timeout, or a custom HTTP method.

-}
request :
    { method : String
    , accessToken : Maybe AccessToken
    , baseUrl : HomeserverUrl
    , path : List String
    , query : List ( String, Maybe String )
    , body : Maybe E.Value
    , expect : Expect msg
    , timeout : Maybe Duration
    , tracker : Maybe String
    }
    -> Cmd msg
request data =
    Http.request
        { method = data.method
        , headers =
            case Maybe.map AccessToken.toString data.accessToken of
                Just s ->
                    [ Http.header "Authorization" ("Bearer " ++ s) ]

                Nothing ->
                    []
        , url = HomeserverUrl.atPath data.path data.query data.baseUrl
        , body =
            data.body
                |> Maybe.map Http.jsonBody
                |> Maybe.withDefault Http.emptyBody
        , expect =
            case data.expect of
                Expect e ->
                    e
        , timeout = Maybe.map Duration.inMilliseconds data.timeout
        , tracker = data.tracker
        }



-- TODO: Perhaps move elsewhere?


{-| Decode a URL from JSON.

This is a small helper used by some API responses that return URLs as strings.

-}
urlDecoder : D.Decoder Url
urlDecoder =
    D.andThen
        (\s ->
            case Url.fromString s of
                Just url ->
                    D.succeed url

                Nothing ->
                    D.fail "Could not convert string to URL"
        )
        D.string

module Internal.Api.Request exposing
    ( ApiCall, ApiPlan, Attribute, callAPI, withAttributes, toChain
    , Request, Error(..)
    , accessToken, timeout, onStatusCode
    , fullBody, bodyBool, bodyInt, bodyString, bodyValue, bodyOpBool, bodyOpInt, bodyOpString, bodyOpValue
    , queryBool, queryInt, queryString, queryOpBool, queryOpInt, queryOpString
    )

{-|


# API module

This module helps describe API requests.


## Plan

@docs ApiCall, ApiPlan, Attribute, callAPI, withAttributes, toChain

Sometimes, APIs might fail. As a result, you may receive an error.

@docs Request, Error


## API attributes


### General attributes

@docs accessToken, timeout, onStatusCode


### Body

@docs fullBody, bodyBool, bodyInt, bodyString, bodyValue, bodyOpBool, bodyOpInt, bodyOpString, bodyOpValue


### Query parameters

@docs queryBool, queryInt, queryString, queryOpBool, queryOpInt, queryOpString

-}

import Dict
import Http
import Internal.Api.Chain as C
import Internal.Config.Log exposing (Log, log)
import Internal.Config.Text as Text
import Internal.Tools.Json as Json
import Internal.Values.Context as Context exposing (APIContext)
import Json.Decode as D
import Json.Encode as E
import Task
import Url
import Url.Builder as UrlBuilder


{-| The API call is a plan that describes how an interaction is planned with
the Matrix API.
-}
type alias ApiCall ph =
    { attributes : List ContextAttr
    , baseUrl : String
    , context : APIContext ph
    , method : String
    , path : List String
    }


{-| Shortcut definition to define a function that bases an APICall on a given
APIContext.
-}
type alias ApiPlan a =
    APIContext a -> ApiCall a


{-| An attribute maps a given context to an attribute for an API call.
-}
type alias Attribute a =
    APIContext a -> ContextAttr


{-| A context attribute describes one aspect of the API call that is to be made.
-}
type ContextAttr
    = BodyParam String Json.Value
    | FullBody Json.Value
    | Header Http.Header
    | NoAttr
    | QueryParam UrlBuilder.QueryParameter
    | StatusCodeResponse Int ( Error, List Log )
    | Timeout Float


{-| Error indicating that something went wrong.
-}
type Error
    = InternetException Http.Error
    | MissingUsername
    | MissingPassword
    | NoSupportedVersion
    | ServerReturnsBadJSON String
    | ServerReturnsError String Json.Value


{-| Ordinary shape of an HTTP request.
-}
type alias Request x a =
    { headers : List Http.Header
    , body : Http.Body
    , method : String
    , url : String
    , resolver : Http.Resolver x a
    , timeout : Maybe Float
    }


{-| Attribute that requires an access token to be present
-}
accessToken : Attribute { a | accessToken : () }
accessToken =
    Context.getAccessToken
        >> (++) "Bearer "
        >> Http.header "Authorization"
        >> Header


{-| Attribute that adds a boolean value to the HTTP body.
-}
bodyBool : String -> Bool -> Attribute a
bodyBool key value =
    bodyValue key <| Json.encode Json.bool value


{-| Attribute that adds an integer value to the HTTP body.
-}
bodyInt : String -> Int -> Attribute a
bodyInt key value =
    bodyValue key <| Json.encode Json.int value


{-| Attribute that adds a boolean to the HTTP body if it is given.
-}
bodyOpBool : String -> Maybe Bool -> Attribute a
bodyOpBool key value =
    case value of
        Just v ->
            bodyBool key v

        Nothing ->
            empty


{-| Attribute that adds an integer value to the HTTP body if it is given.
-}
bodyOpInt : String -> Maybe Int -> Attribute a
bodyOpInt key value =
    case value of
        Just v ->
            bodyInt key v

        Nothing ->
            empty


{-| Attribute that adds a string value to the HTTP body if it is given.
-}
bodyOpString : String -> Maybe String -> Attribute a
bodyOpString key value =
    case value of
        Just v ->
            bodyString key v

        Nothing ->
            empty


{-| Attribute that adds a JSON value to the HTTP body if it is given.
-}
bodyOpValue : String -> Maybe Json.Value -> Attribute a
bodyOpValue key value =
    case value of
        Just v ->
            bodyValue key v

        Nothing ->
            empty


{-| Attribute that adds a string value to the HTTP body.
-}
bodyString : String -> String -> Attribute a
bodyString key value =
    bodyValue key <| Json.encode Json.string value


{-| Attribute that adds a JSON value to the HTTP body.
-}
bodyValue : String -> Json.Value -> Attribute a
bodyValue key value _ =
    BodyParam key value


{-| Create a plan to create an API call.
-}
callAPI : { method : String, path : List String } -> ApiPlan { a | baseUrl : () }
callAPI { method, path } context =
    { attributes = []
    , baseUrl = Context.getBaseUrl context
    , context = context
    , method = method
    , path = path
    }


{-| Decode the server's response into (hopefully) something meaningful.
-}
decodeServerResponse : D.Decoder ( a, List Log ) -> String -> Maybe ( Error, List Log ) -> Result ( Error, List Log ) ( a, List Log )
decodeServerResponse decoder body statusCodeError =
    case D.decodeString D.value body of
        Err e ->
            let
                description : String
                description =
                    D.errorToString e
            in
            Err
                ( ServerReturnsBadJSON description
                , description
                    |> Text.logs.serverReturnedInvalidJSON
                    |> log.error
                    |> List.singleton
                )

        Ok v ->
            decodeServerValue decoder v statusCodeError


{-| Decode the server's response, assuming that it parses correctly to
a JSON value.
-}
decodeServerValue : D.Decoder ( a, List Log ) -> Json.Value -> Maybe ( Error, List Log ) -> Result ( Error, List Log ) ( a, List Log )
decodeServerValue decoder value statusCodeError =
    value
        |> D.decodeValue decoder
        |> Result.mapError
            (\err ->
                let
                    description : String
                    description =
                        D.errorToString err

                    -- TODO: Parse errors returned by Matrix API
                    error : Maybe ( Error, List Log )
                    error =
                        Nothing
                in
                case ( error, statusCodeError ) of
                    ( Just e, _ ) ->
                        e

                    ( Nothing, Just e ) ->
                        e

                    ( Nothing, Nothing ) ->
                        ( ServerReturnsBadJSON description
                        , description
                            |> Text.logs.serverReturnedUnknownJSON
                            |> log.error
                            |> List.singleton
                        )
            )


{-| Add an empty attribute that does nothing.
-}
empty : Attribute a
empty =
    always NoAttr


{-| Adds a JSON value as the HTTP body.
-}
fullBody : Json.Value -> Attribute a
fullBody value _ =
    FullBody value


getBody : List ContextAttr -> Json.Value
getBody attributes =
    attributes
        |> List.filterMap
            (\attr ->
                case attr of
                    FullBody v ->
                        Just v

                    _ ->
                        Nothing
            )
        |> List.reverse
        |> List.head
        |> Maybe.withDefault
            (List.filterMap
                (\attr ->
                    case attr of
                        BodyParam key value ->
                            Just ( key, value )

                        _ ->
                            Nothing
                )
                attributes
                |> E.object
            )


getHeaders : List ContextAttr -> List Http.Header
getHeaders =
    List.filterMap
        (\attr ->
            case attr of
                Header h ->
                    Just h

                _ ->
                    Nothing
        )


getQueryParams : List ContextAttr -> List UrlBuilder.QueryParameter
getQueryParams =
    List.filterMap
        (\attr ->
            case attr of
                QueryParam q ->
                    Just q

                _ ->
                    Nothing
        )


getStatusCodes : List ContextAttr -> Dict.Dict Int ( Error, List Log )
getStatusCodes =
    List.filterMap
        (\attr ->
            case attr of
                StatusCodeResponse code err ->
                    Just ( code, err )

                _ ->
                    Nothing
        )
        >> Dict.fromList


getTimeout : List ContextAttr -> Maybe Float
getTimeout =
    List.filterMap
        (\attr ->
            case attr of
                Timeout f ->
                    Just f

                _ ->
                    Nothing
        )
        >> List.reverse
        >> List.head


getUrl : ApiCall a -> String
getUrl { attributes, baseUrl, path } =
    UrlBuilder.crossOrigin
        baseUrl
        (List.map Url.percentEncode path)
        (getQueryParams attributes)


{-| When the HTTP request cannot be deciphered but the status code is known,
return with a given default error.
-}
onStatusCode : Int -> String -> Attribute a
onStatusCode code err _ =
    StatusCodeResponse code
        ( err
            |> E.string
            |> Tuple.pair "errcode"
            |> List.singleton
            |> E.object
            |> ServerReturnsError err
        , String.concat
            -- TODO: Move to Internal.Config.Text
            [ "Received an invalid HTTP response from Matrix server "
            , "but managed to decode it using the status code "
            , String.fromInt code
            , ": Default to errcode "
            , err
            ]
            |> log.warn
            |> List.singleton
        )


{-| Add a boolean value as a query parameter to the URL.
-}
queryBool : String -> Bool -> Attribute a
queryBool key value _ =
    (if value then
        "true"

     else
        "false"
    )
        |> UrlBuilder.string key
        |> QueryParam


{-| Add an integer value as a query parameter to the URL.
-}
queryInt : String -> Int -> Attribute a
queryInt key value _ =
    QueryParam <| UrlBuilder.int key value


{-| Add a boolean value as a query parameter to the URL if it exists.
-}
queryOpBool : String -> Maybe Bool -> Attribute a
queryOpBool key value =
    case value of
        Just v ->
            queryBool key v

        Nothing ->
            empty


{-| Add an integer value as a query parameter to the URL if it exists.
-}
queryOpInt : String -> Maybe Int -> Attribute a
queryOpInt key value =
    case value of
        Just v ->
            queryInt key v

        Nothing ->
            empty


{-| Add a string value as a query parameter to the URL if it exists.
-}
queryOpString : String -> Maybe String -> Attribute a
queryOpString key value =
    case value of
        Just v ->
            queryString key v

        Nothing ->
            empty


{-| Add a string value as a query parameter to the URL.
-}
queryString : String -> String -> Attribute a
queryString key value _ =
    QueryParam <| UrlBuilder.string key value


{-| Resolve the response of a Matrix API call.
-}
rawApiCallResolver : D.Decoder ( a, List Log ) -> Dict.Dict Int ( Error, List Log ) -> Http.Resolver ( Error, List Log ) ( a, List Log )
rawApiCallResolver decoder statusCodeErrors =
    Http.stringResolver
        (\response ->
            case response of
                Http.BadUrl_ s ->
                    Http.BadUrl s
                        |> InternetException
                        |> Tuple.pair
                        |> (|>) []
                        |> Err

                Http.Timeout_ ->
                    Http.Timeout
                        |> InternetException
                        |> Tuple.pair
                        |> (|>) []
                        |> Err

                Http.NetworkError_ ->
                    Http.NetworkError
                        |> InternetException
                        |> Tuple.pair
                        |> (|>) []
                        |> Err

                Http.BadStatus_ metadata body ->
                    statusCodeErrors
                        |> Dict.get metadata.statusCode
                        |> decodeServerResponse decoder body

                Http.GoodStatus_ metadata body ->
                    statusCodeErrors
                        |> Dict.get metadata.statusCode
                        |> decodeServerResponse decoder body
        )


{-| Configure the HTTP request to time out after a given expiry time.
-}
timeout : Float -> Attribute a
timeout f _ =
    Timeout f


{-| Transform an APICall to a TaskChain.
-}
toChain :
    { logHttp : Request ( Error, List Log ) ( update, List Log ) -> ( update, List Log )
    , coder : Json.Coder httpOut
    , request : ApiPlan ph1
    , toContextChange : httpOut -> (APIContext ph1 -> APIContext ph2)
    , toUpdate : httpOut -> ( update, List Log )
    }
    -> C.TaskChain Error update ph1 ph2
toChain data apiContext =
    data.request apiContext
        |> (\call ->
                let
                    r : Request ( Error, List Log ) ( httpOut, List Log )
                    r =
                        { method = call.method
                        , headers = getHeaders call.attributes
                        , url = getUrl call
                        , body = Http.jsonBody (getBody call.attributes)
                        , resolver = rawApiCallResolver (Json.decode data.coder) (getStatusCodes call.attributes)
                        , timeout = getTimeout call.attributes
                        }

                    logR : Request ( Error, List Log ) ( update, List Log )
                    logR =
                        { method = call.method
                        , headers = getHeaders call.attributes
                        , url = getUrl call
                        , body = Http.jsonBody (getBody call.attributes)
                        , resolver =
                            rawApiCallResolver
                                (Json.decode data.coder
                                    |> D.map
                                        (\( out, logs ) ->
                                            case data.toUpdate out of
                                                ( u, uLogs ) ->
                                                    ( u, List.append logs uLogs )
                                        )
                                )
                                (getStatusCodes call.attributes)
                        , timeout = getTimeout call.attributes
                        }
                in
                case data.logHttp logR of
                    ( httpU, httpLogs ) ->
                        Http.task r
                            |> Task.map
                                (\( httpO, logs ) ->
                                    case data.toUpdate httpO of
                                        ( u, uLogs ) ->
                                            { contextChange = data.toContextChange httpO
                                            , logs = List.concat [ httpLogs, logs, uLogs ]
                                            , messages = [ httpU, u ]
                                            }
                                )
                            |> Task.mapError
                                (\( err, logs ) ->
                                    { error = err
                                    , logs = List.append httpLogs logs
                                    , messages = [ httpU ]
                                    }
                                )
           )


{-| Add more attributes to the API plan.
-}
withAttributes : List (Attribute a) -> ApiPlan a -> ApiPlan a
withAttributes attrs f context =
    f context
        |> (\data ->
                { data
                    | attributes =
                        attrs
                            |> List.map (\attr -> attr data.context)
                            |> List.append data.attributes
                }
           )

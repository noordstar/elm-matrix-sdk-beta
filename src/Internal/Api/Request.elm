module Internal.Api.Request exposing
    ( ApiCall, ApiPlan, callAPI, withAttributes
    , accessToken, withTransactionId
    , fullBody, bodyBool, bodyInt, bodyString, bodyValue, bodyOpBool, bodyOpInt, bodyOpString, bodyOpValue
    , queryBool, queryInt, queryString, queryOpBool, queryOpInt, queryOpString
    )

{-|


# API module

This module helps describe API requests.


## Plan

@docs ApiCall, ApiPlan, callAPI, withAttributes


## API attributes


### General attributes

@docs accessToken, withTransactionId


### Body

@docs fullBody, bodyBool, bodyInt, bodyString, bodyValue, bodyOpBool, bodyOpInt, bodyOpString, bodyOpValue


### Query parameters

@docs queryBool, queryInt, queryString, queryOpBool, queryOpInt, queryOpString

-}

import Http
import Internal.Tools.Json as Json
import Internal.Values.Context as Context exposing (APIContext)
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
    | ReplaceInUrl String String
    | Timeout Float
    | UrlPath String


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
    { attributes =
        path
            |> List.map Url.percentEncode
            |> String.join "/"
            |> (++) "/"
            |> UrlPath
            |> List.singleton
    , baseUrl = Context.getBaseUrl context
    , context = context
    , method = method
    }


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


{-| Attribute that requires a transaction id to be present.
-}
withTransactionId : Attribute { a | transaction : () }
withTransactionId =
    Context.getTransaction >> ReplaceInUrl "txnId"

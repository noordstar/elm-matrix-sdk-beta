module Matrix.Types.HomeserverUrl exposing
    ( HomeserverUrl, atPath, fromString, fromUrl, toString, toUrl
    , encode, decoder
    )

{-| Homeserver base URLs.

This type stores the protocol, host, optional port and optional path prefix of
a homeserver. Most homeservers live at the root of their domain, such as
`https://matrix.example.org`, but the spec also allows a path prefix, such as
`https://example.org/matrix`. Query and fragment are always dropped, so you
can safely build Matrix API URLs from a known base.


## HomeserverUrl

@docs HomeserverUrl, atPath, fromString, fromUrl, toString, toUrl


## JSON

@docs encode, decoder

-}

import Json.Decode as D
import Json.Encode as E
import Url
import Url.Builder


{-| A homeserver URL with only base components.
-}
type HomeserverUrl
    = HomeserverUrl
        { protocol : Url.Protocol
        , host : String
        , port_ : Maybe Int
        , pathPrefix : List String
        }


{-| Build an absolute URL at a given path and query.

Path segments are percent-encoded automatically, and they are added after the
homeserver's path prefix, if it has one. Only query parameters with a `Just`
value are included.

    atPath [ "_matrix", "client", "v3", "sync" ]
        [ ( "timeout", Just "30000" ), ( "since", Nothing ) ]
        homeserver
    -- "https://example.org/_matrix/client/v3/sync?timeout=30000"

Note: This function does not validate whether the path is a real Matrix
endpoint. It just builds the URL.

-}
atPath : List String -> List ( String, Maybe String ) -> HomeserverUrl -> String
atPath path query ((HomeserverUrl data) as hurl) =
    let
        url =
            toUrl hurl
    in
    Url.toString
        { url
            | path = "/" ++ String.join "/" (data.pathPrefix ++ List.map Url.percentEncode path)
            , query =
                query
                    |> List.filterMap
                        (\( key, mvalue ) -> Maybe.map (Url.Builder.string key) mvalue)
                    |> (\items ->
                            case items of
                                [] ->
                                    Nothing

                                _ :: _ ->
                                    Just items
                       )
                    |> Maybe.map (Url.Builder.toQuery >> String.dropLeft 1)
        }


{-| Decode a homeserver URL from a JSON string.

    import Json.Decode as D
    import Matrix.Types.HomeserverUrl as HomeserverUrl

    D.decodeString HomeserverUrl.decoder "\"https://example.org\""
    -- Ok <homeserverUrl>

Fails if the value is not a valid URL.

-}
decoder : D.Decoder HomeserverUrl
decoder =
    D.string
        |> D.andThen
            (\s ->
                case Url.fromString s of
                    Just u ->
                        D.succeed u

                    Nothing ->
                        D.fail "Cannot convert homeserver string to URL"
            )
        |> D.map fromUrl


{-| Encode a homeserver URL as a JSON string.

    HomeserverUrl.encode homeserver

-}
encode : HomeserverUrl -> E.Value
encode =
    toString >> E.string


{-| Parse a homeserver URL from a string.

    HomeserverUrl.fromString "https://example.org"
    -- Just <homeserverUrl>

    HomeserverUrl.fromString "https://example.org/matrix/"
    -- Just <homeserverUrl>, with the path prefix "/matrix"

    HomeserverUrl.fromString "not a url"
    -- Nothing

-}
fromString : String -> Maybe HomeserverUrl
fromString =
    Url.fromString >> Maybe.map fromUrl


{-| Create a `HomeserverUrl` from `Url.Url`.

The path becomes the homeserver's path prefix. Trailing slashes don't matter:
`https://example.org/matrix` and `https://example.org/matrix/` give the same
result. The query and fragment are dropped.

-}
fromUrl : Url.Url -> HomeserverUrl
fromUrl url =
    HomeserverUrl
        { protocol = url.protocol
        , host = url.host
        , port_ = url.port_
        , pathPrefix =
            url.path
                |> String.split "/"
                |> List.filter (not << String.isEmpty)
        }


{-| Convert a homeserver URL to string form.

    HomeserverUrl.toString homeserver
    -- "https://example.org/"

-}
toString : HomeserverUrl -> String
toString =
    toUrl >> Url.toString


{-| Convert to `Url.Url`. The path is the homeserver's path prefix, or `/` if
it has none.

This is useful when you need to use functions from `elm/url`.

-}
toUrl : HomeserverUrl -> Url.Url
toUrl (HomeserverUrl data) =
    { protocol = data.protocol
    , host = data.host
    , port_ = data.port_
    , path = "/" ++ String.join "/" data.pathPrefix
    , query = Nothing
    , fragment = Nothing
    }

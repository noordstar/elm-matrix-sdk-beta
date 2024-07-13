module Internal.Tools.StrippedEvent exposing (StrippedEvent, coder, strip)

{-|


# Stripped event

The stripped event is a simple Matrix event that does not contain any metadata.

@docs StrippedEvent, coder, strip

-}

import Internal.Config.Text as Text
import Internal.Tools.Json as Json


type alias StrippedEvent =
    { content : Json.Value, eventType : String }


coder : Json.Coder StrippedEvent
coder =
    Json.object2
        { name = Text.docs.strippedEvent.name
        , description = Text.docs.strippedEvent.description
        , init = StrippedEvent
        }
        (Json.field.required
            { fieldName = "content"
            , toField = .content
            , description =
                [ "Event content"
                ]
            , coder = Json.value
            }
        )
        (Json.field.required
            { fieldName = "type"
            , toField = .eventType
            , description =
                [ "Event type, generally namespaced using the Java package naming convention."
                ]
            , coder = Json.string
            }
        )


strip : { a | content : Json.Value, eventType : String } -> StrippedEvent
strip { content, eventType } =
    { content = content, eventType = eventType }

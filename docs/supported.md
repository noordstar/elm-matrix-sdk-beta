# Supported features in spec versions

The Matrix spec frequently releases new spec versions. Most features are
supported in the Elm SDK on most spec releases, but not all endpoints exist
for every version. For example, in very early versions you couldn't even get
the content of an event!

This document specifies which features you can expect to work for which spec
versions. This can help you decide whether the SDK will interoperate with your
homeserver, or that it may be better to not rely on a specific feature.

---

The following table describes which versions are supported by which endpoint.
The table assumes an order. For the Matrix spec, spec versions are usually
ordered as following:

(oldest) `r0.0.0`, `r0.0.1`, `r0.1.0`, `r0.2.0`, `r0.3.0`, `r0.4.0`, `r0.5.0`,
`r0.6.0`, `r0.6.1`, `v1.1`, `v1.2`, `v1.3`, `v1.4`, `v1.5`, `v1.6`, `v1.7`,
`v1.8`, `v1.9`, `v1.10`, `v1.11`, `v1.12` (newer)

| Elm SDK function             | Spec versions    | Notes |
| ---------------------------- | ---------------- | ----- |
| Matrix.leave                 | all              |       |
| Matrix.sendMessageEvent      | all              |       |
| Matrix.setAccountData        | all              |       |
| Matrix.sync                  | v1.1 and newer   | The `/sync` endpoint is very complex and hence tedious to implement. For this reason, the legacy versions have not yet received sync support. |
| Matrix.whoAmI                | r0.3.0 and newer | This endpoint did not exist before version `r0.3.0`. |
| Matrix.Event.redact          | all              |       |
| Matrix.Invite.accept         | all              |       |
| Matrix.Invite.reject         | all              |       |
| Matrix.Room.ban              | all              |       |
| Matrix.Room.invite           | all              |       |
| Matrix.Room.kick             | all              | The `/kick` endpoint was added in `r0.1.0`. On older servers, the Elm SDK sends a custom membership state event. |
| Matrix.Room.leave            | all              |       |
| Matrix.Room.redact           | all              |       |
| Matrix.Room.sendMessageEvent | all              |       |
| Matrix.Room.sendStateEvent   | all              |       |
| Matrix.Room.setAccountData   | all              |       |

# elm-meld
Composeable `Task`s, instead of hundreds of Msg pattern match cases.

elm-meld provides `Meld m x msg` which wraps a model (m) and error (x) and a
message type (msg).

`Meld m x msg` starts with a model, collects `Task`s, to be executed by Elm's
runtime, collects model _merge_ functions, to process `Task` execution results,
and collects _command_ functions (`m -> Cmd msg`) to be executed after
model merges.

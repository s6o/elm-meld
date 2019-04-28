# elm-meld

Write your Model-View-Update's update actions as `Task`s to be executed at
once or sequencially. Pass along incremental Model updates to a next `Task` in a
sequence.

The main idea of elm-meld can be codified with the following function and type signatures:

```elm
    Meld m x msg -> Task (Meld.Error m x) (Meld m x msg)

    type alias MeldTask m x msg =
        Task (Meld.Error x m) (Meld m msg)

    type alias MeldResponse m x msg =
        Result (Error m x) (Meld m x msg)
```

where `m` represents an application's model and `msg` the union type used for
messages/tagging and `x` is the union type for errors.

The `Meld m x msg` starts with a model, collects `Task`s, to be executed by Elm's
runtime, collects model _merge_ functions, to process `Task` execution results,
and collects _command_ functions (`m -> Cmd msg`) to be executed after
model merges.

## Setting up a 'Msg'

```elm
import Http
import Meld exposing (MeldTask, Error(..))

type MError
    = EMsg String
    | EHttp Http.Error

type Model =
    { errorMessage : String
    , credentials : Maybe Credentials
    -- ..additional fields
    }

type Msg
    = Act (List (Meld Model MError Msg -> MeldTask Model MError Msg)
    | ActSeq (List (Meld Model Merror Msg -> MeldTask Model MError Msg))
    | Responses (MeldResponse Model MError Msg)
    | TextInput (String -> Meld Model MError Msg -> MeldTask Model MError Msg) String


modelErrMsg : MError -> String
modelErrMsg merror  =
    case merror of
        EMsg s ->
            s

        EHttp hErr ->
            case hErr of
                BadUrl s ->
                    "Bad URL - " ++ s

                Timeout s ->
                    "Http request timeout."

                NetworkError ->
                    "Network failure."

                BadStatus scode ->
                    "Bad status - " ++ (String.fromInt scode)

                BadBody s ->
                    "Bad body - " ++ s
```

## Update

```elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Act tasks ->
            Meld.init model
                |> Meld.addTasks tasks
                |> Meld.send Responses

        ActSeq tasks ->
            Meld.init model
                |> Meld.addTasks tasks
                |> Meld.sequence Responses

        Responses result ->
            case result of
                Ok meld ->
                    Meld.update model meld

                Err meldError ->
                    let
                        errModel =
                            Meld.errorModel meldError
                    ( {errModel | errorMessage = Meld.errorMessage modelErrMsg meldError}
                    , Cmd.none
                    )

        TextInput task input ->
            Meld.init
                model
                |> Meld.addTasks [ task input ]
                |> Meld.send Responses

```

## A data module

```elm
module Credentials exposing
    ( Credentials
    , CredentialsField(..)
    , State(..)
    , stateTo
    , update
    , validate
    )

import MError exposing (MError(..))

type alias Parent m =
    { m | credentials : Maybe Credentials }


type alias Credentials =
    { username : String
    , password : String
    , state : State
    }


type CredentialsField
    = Username
    | Password


type State
    = Failure String
    | Processing
    | WaitingEntries


{-| Credentials state update `Task`.
-}
stateTo : State -> Meld (Parent m) x msg -> MeldTask (Parent m) x msg
stateTo newState meld =
    let
        taskModel ma =
            { ma
                | credentials =
                    ma.credentials |> Maybe.map (\r -> { r | state = newState })
            }
    in
    Meld.withMerge taskModel meld
        |> Task.succeed


{-| Credentials record update `Task`.
-}
update : CredentialsField -> String -> Meld (Parent m) x msg -> MeldTask (Parent m) x msg
update field value meld =
    let
        model =
            Meld.model meld

        taskModel ma =
            { ma
                | credentials =
                    ma.credentials
                        |> (\mc ->
                                if EMaybe.isNothing mc then
                                    empty
                                else
                                    mc
                           )
                        |> Maybe.map (updateField field value)
            }
    in
    Meld.withMerge taskModel meld
        |> Task.succeed


{-| Credentials validaion `Task`, to be chained before an HTTP `Task`.
-}
validate : Meld (Parent m) x msg -> MeldTask (Parent m) x msg
validate meld =
    let
        model =
            Meld.model meld
    in
    model.credentials
        |> Result.fromMaybe "Missing credentials from model."
        |> Result.andThen
            (\r ->
                if String.length r.username > 0 then
                    Ok meld
                else
                    Err "Username too short."
            )
        |> Result.andThen
            (\r ->
                if String.length r.password > 0 then
                    Ok meld
                else
                    Err "Password too short."
            )
        |> Result.mapError (\s -> EMsg s |> Meld.error meld)


{-| @private
-}
empty : Maybe Credentials
empty =
    { username = ""
    , password = ""
    , state = WaitingEntries
    }
        |> Just


{-| @private
-}
updateField : CredentialsField -> String -> Credentials -> Credentials
updateField field value r =
    case field of
        Username ->
            { r | state = WaitingEntries, username = value }

        Password ->
            { r | state = WaitingEntries, password = value }
```

## A View

```elm
import Credentials as Creds

-- ...

view : Model -> Html Msg
view model =
-- ...
    input
        [ TextInput (Creds.update Creds.Username) |> onClick
        ]
        []
```
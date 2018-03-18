# elm-meld
Composeable `Task`s, instead of just separate 'Msg' pattern match cases.

The main idea of elm-meld can be codified with the following function signature:

```elm
        Meld m msg -> Task (Meld.Error m) (Meld m msg)
```

where `m` represents an application's model and `msg` the union type used for
messages/tagging.

The `Meld m msg` starts with a model, collects `Task`s, to be executed by Elm's
runtime, collects model _merge_ functions, to process `Task` execution results,
and collects _command_ functions (`m -> Cmd msg`) to be executed after
model merges.

## Setting up a 'Msg'

```elm
type Msg
    = Act (List (Meld Model Msg -> Task (Error Model) (Meld Model Msg)))
    | ActSeq (List (Meld Model Msg -> Task (Error Model) (Meld Model Msg)))
    | Responses (Result (Error Model) (Meld Model Msg))
    | TextInput (String -> Meld Model Msg -> Task (Error Model) (Meld Model Msg)) String
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
                        _ =
                            Debug.log "Act/ActSeq Error" <| Meld.errorMessage meldError
                    in
                    ( Meld.errorModel meldError
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
{-| Parent `Model` requirements.
-}
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


stateTo : State -> Meld (Parent m) msg -> Task (Error (Parent m)) (Meld (Parent m) msg)
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


update : CredentialsField -> String -> Meld (Parent m) msg -> Task (Error (Parent m)) (Meld (Parent m) msg)
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


validate : Meld (Parent m) msg -> Task (Error (Parent m)) (Meld (Parent m) msg)
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
                    Ok r
                else
                    Err "Username too short."
            )
        |> Result.andThen
            (\r ->
                if String.length r.password > 0 then
                    Ok r
                else
                    Err "Password too short."
            )
        |> (\result ->
                case result of
                    Err msg ->
                        EMsg model msg |> Task.fail

                    Ok _ ->
                        Task.succeed meld
           )
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
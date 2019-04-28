module Meld exposing
    ( Meld, init
    , Merge, withMerge, withCmds, model
    , MeldTask, MeldResponse, addTasks, send, sequence, update
    , Error(..), errorModel, errorMessage
    , cmds, cmdseq
    )

{-| Write your Model-View-Update's update actions as `Task`s to be executed at
once or sequencially. Pass along incremental Model updates to a next `Task` in a
sequence.


# Initialization

@docs Meld, init


# Task Postprocessing Definitions

@docs Merge, withMerge, withCmds, model


# Application Integration

@docs MeldTask, MeldResponse, addTasks, send, sequence, update


# Error Handling

@docs Error, errorModel, errorMessage


# Command Management

@docs cmds, cmdseq

-}

import Task exposing (Task)



-- Initialization


{-| Capture an application's model and tasks, process results with model merges and commands.
-}
type Meld m x msg
    = Meld
        { appModel : m
        , tasks : List (Meld m x msg -> MeldTask m x msg)
        , merges : List (m -> m)
        , commands : List (m -> Cmd msg)
        }


{-| Create an initial `Meld m msg` from specified application model.
-}
init : m -> Meld m x msg
init m =
    Meld
        { appModel = m
        , tasks = []
        , merges = []
        , commands = []
        }



-- Task Post-processing Definitions with `Merge m` and `MCmd m msg`


{-| -}
type alias Merge m =
    m -> m


{-| Apply `Merge m` to current `m` in `Meld m msg` and store it for `update`.
-}
withMerge : Merge m -> Meld m x msg -> Meld m x msg
withMerge mergeFn (Meld r) =
    Meld
        { r
            | appModel = mergeFn r.appModel
            , merges = mergeFn :: r.merges
        }


{-| Append a list of command functions to specified `Meld m msg` for `update`.
-}
withCmds : List (m -> Cmd msg) -> Meld m x msg -> Meld m x msg
withCmds cmdFns (Meld r) =
    Meld
        { r | commands = r.commands ++ cmdFns }


{-| Unpack `Meld m msg`'s model.
-}
model : Meld m x msg -> m
model (Meld { appModel }) =
    appModel



-- Application Intergration


{-| -}
type alias MeldTask m x msg =
    Task (Error m x) (Meld m x msg)


{-| -}
type alias MeldResponse m x msg =
    Result (Error m x) (Meld m x msg)


{-| Append a list of task functions to specified `Meld m msg`.
-}
addTasks : List (Meld m x msg -> MeldTask m x msg) -> Meld m x msg -> Meld m x msg
addTasks taskFns (Meld r) =
    Meld
        { r | tasks = r.tasks ++ taskFns }


{-| Create `Cmd`s from `Meld m msg` tasks to be executed in any/unspecified order.
-}
send : (MeldResponse m x msg -> msg) -> Meld m x msg -> ( m, Cmd msg )
send toMsg (Meld r) =
    if List.isEmpty r.tasks then
        ( r.appModel
        , Cmd.none
        )

    else
        ( r.appModel
        , cmds toMsg (Meld r)
        )


{-| Execute `Meld m msg` tasks in sequence, by continueing to a next task
only upon successful completion of the previous task.
-}
sequence : (MeldResponse m x msg -> msg) -> Meld m x msg -> ( m, Cmd msg )
sequence toMsg (Meld r) =
    if List.isEmpty r.tasks then
        ( r.appModel
        , Cmd.none
        )

    else
        ( r.appModel
        , cmdseq toMsg (Meld r)
        )


{-| Apply model merges to `m` and create command batch.
-}
update : m -> Meld m x msg -> ( m, Cmd msg )
update appModel (Meld { merges, commands }) =
    let
        finalModel =
            merges
                |> List.foldr (\mergeFn accumModel -> mergeFn accumModel) appModel
    in
    ( finalModel
    , commands
        |> List.map (\cmdFn -> cmdFn finalModel)
        |> Cmd.batch
    )



-- Error Handling


{-| Common error handling type to ease composability.
-}
type Error m x
    = ErrX m x


{-| Unpack `Error m`'s model.
-}
errorModel : Error m x -> m
errorModel error =
    case error of
        ErrX m _ ->
            m


{-| Retrive the error message.
-}
errorMessage : (x -> String) -> Error m x -> String
errorMessage xFn error =
    case error of
        ErrX _ x ->
            xFn x



-- CMD Management


{-| Execute a set of `Meld m msg`'s tasks in any/unspecified order.
-}
cmds : (MeldResponse m x msg -> msg) -> Meld m x msg -> Cmd msg
cmds toMsg meld =
    let
        (Meld { tasks }) =
            meld
    in
    tasks
        |> List.map (\tf -> tf meld |> Task.attempt toMsg)
        |> Cmd.batch


{-| Execute a set of `Meld m msg`'s tasks in sequence, by proceeding to the next
only upon successful execution.
-}
cmdseq : (MeldResponse m x msg -> msg) -> Meld m x msg -> Cmd msg
cmdseq toMsg meld =
    let
        (Meld { tasks }) =
            meld
    in
    tasks
        |> List.foldl Task.andThen (initTask meld)
        |> Task.attempt toMsg


{-| @private
Default initial/first `Task` for a sequnce of tasks.
-}
initTask : Meld m x msg -> Task (Error m x) (Meld m x msg)
initTask meld =
    Task.succeed meld

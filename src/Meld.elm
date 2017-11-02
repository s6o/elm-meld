module Types.Meld
    exposing
        ( FnCmd
        , FnMerge
        , FnTask
        , Meld
        , Model
        , cmds
        , cmdseq
        , finalize
        , init
        , model
        , sequence
        , update
        , withCmds
        , withMerge
        , withModel
        , withTasks
        )

{-| Composable `Task`s, instead of hundreds of Msg pattern match cases.

@docs FnCmd, FnMerge, FnTask, Meld, Model, init, sequence, update, withMergeCmds, withTasks

-}

import Task exposing (Task)


{-| `Meld r x`'s command signature.
-}
type alias FnCmd r msg =
    Model r -> Cmd msg


{-| `Meld r x`'s merge signature.
-}
type alias FnMerge r =
    Model r -> Model r


{-| `Meld r x`'s task signature.
-}
type alias FnTask r x msg =
    Meld r x msg -> Task x (Meld r x msg)


{-| An application's `Model r`, `FnTask r x` and `FnCmd rx` container to manage
Elm's `Task` composition and application model merges.
-}
type Meld r x msg
    = Meld
        { error : x
        , model : Model r
        , tasks : List (FnTask r x msg)
        , merges : List (FnMerge r)
        , commands : List (FnCmd r msg)
        }


{-| Requirement(s) for an application's model
-}
type alias Model r =
    { r | meldTasks : Int }


{-| Execute a set of `Meld r x`'s tasks in any/unspecified order.
-}
cmds : (Result x (Meld r x msg) -> msg) -> Meld r x msg -> Cmd msg
cmds toMsg meld =
    let
        (Meld { tasks }) =
            meld
    in
    tasks
        |> List.map (\tf -> tf meld |> Task.attempt toMsg)
        |> Cmd.batch


{-| Execute a set of `Meld r x`'s tasks in sequence, by proceeding to the next
only upon successful execution.
-}
cmdseq : (Result x (Meld r x msg) -> msg) -> Meld r x msg -> Cmd msg
cmdseq toMsg meld =
    let
        (Meld { tasks }) =
            meld
    in
    tasks
        |> List.foldl Task.andThen (initTask meld)
        |> Task.attempt toMsg


{-| Merge up model changes and execute commands with latest model as specified
by `withMergeCmds`.
-}
finalize : Int -> Model r -> Meld r x msg -> ( Model r, Cmd msg )
finalize taskCount appModel (Meld { merges, commands }) =
    let
        finalModel =
            merges
                |> List.foldr
                    (\mergeFn accumModel -> mergeFn accumModel)
                    { appModel | meldTasks = appModel.meldTasks - taskCount }
    in
    ( finalModel
    , commands
        |> List.map (\cmdFn -> cmdFn finalModel)
        |> Cmd.batch
    )


{-| Create an initial `Meld r x` from specified error and application model types.
-}
init : Model r -> x -> Meld r x msg
init appModel appError =
    Meld
        { error = appError
        , model = appModel
        , tasks = []
        , merges = []
        , commands = []
        }


{-| Default initial/first `Task` for a sequnce of tasks.
-}
initTask : Meld r x msg -> Task x (Meld r x msg)
initTask meld =
    Task.succeed meld


{-| Get application's model.
-}
model : Meld r x msg -> Model r
model (Meld { model }) =
    model


{-| Execute a set of `Task`s added to a `Meld r x` instance in sequence and
update `Model r`'s `meldTasks` counter.
-}
sequence : (Int -> Result x (Meld r x msg) -> msg) -> Meld r x msg -> ( Model r, Cmd msg )
sequence toMsg (Meld { error, model, tasks }) =
    if List.isEmpty tasks then
        ( model
        , Cmd.none
        )
    else
        let
            taskCount =
                List.length tasks

            nextModel =
                { model | meldTasks = model.meldTasks + taskCount }
        in
        ( nextModel
        , init nextModel error
            |> cmdseq (toMsg taskCount)
        )


{-| Execute a set of `Task`s added to a `Meld r x` instance in any order and
update `Model r`'s `meldTasks` counter.
-}
update : (Int -> Result x (Meld r x msg) -> msg) -> Meld r x msg -> ( Model r, Cmd msg )
update toMsg (Meld { error, model, tasks }) =
    if List.isEmpty tasks then
        ( model
        , Cmd.none
        )
    else
        let
            nextModel =
                { model | meldTasks = model.meldTasks + List.length tasks }
        in
        ( nextModel
        , init nextModel error
            |> cmds (toMsg 1)
        )


{-| Append a merge function and a list of command task functions to specified `Meld r x`.
-}
withCmds : List (FnCmd r msg) -> Meld r x msg -> Meld r x msg
withCmds cmdFns (Meld r) =
    Meld
        { r | commands = cmdFns ++ r.commands }


{-| Append a merge function to specified `Meld r x`.
-}
withMerge : FnMerge r -> Meld r x msg -> Meld r x msg
withMerge mergeFn (Meld r) =
    Meld
        { r
            | model = mergeFn r.model
            , merges = mergeFn :: r.merges
        }


{-| Override model in specified `Meld r x`.
-}
withModel : Model r -> Meld r x msg -> Meld r x msg
withModel appModel (Meld r) =
    Meld
        { r | model = appModel }


{-| Append a list of task functions to specified `Meld r x`.
-}
withTasks : List (FnTask r x msg) -> Meld r x msg -> Meld r x msg
withTasks taskFns (Meld r) =
    Meld
        { r | tasks = r.tasks ++ taskFns }

module Types.Meld
    exposing
        ( Meld
        , Model
        , cmds
        , cmdseq
        , init
        , sequence
        , update
        , withMergeCmds
        , withTasks
        )

{-| Composable `Task`s, instead of hundreds of Msg pattern match cases.

@docs Meld, Model, cmds, cmdseq, init, sequence, update, withMergeCmds, withTasks

-}

import Task exposing (Task)


{-| A model, `Task` and `Cmd` container to manage task composition, model
merges and `Cmd`s.
-}
type Meld r x
    = Meld
        { error : x
        , model : Model r
        , tasks : List (Meld r x -> Task x (Meld r x))
        , merges : List (Model r -> Model r)
        , commands : List (Model r -> Task x (Meld r x))
        }


{-| Requirement(s) for an application's model
-}
type alias Model r =
    { r | meldTasks : Int }


{-| Execute a set of `Meld r x`'s tasks in any/unspecified order.
-}
cmds : (Result x (Meld r x) -> msg) -> Meld r x -> Cmd msg
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
cmdseq : (Result x (Meld r x) -> msg) -> Meld r x -> Cmd msg
cmdseq toMsg meld =
    let
        (Meld { tasks }) =
            meld
    in
    tasks
        |> List.foldl Task.andThen (initTask meld)
        |> Task.attempt toMsg


{-| Create an initial `Meld r x` from specified error and application model types.
-}
init : x -> Model r -> Meld r x
init appError appModel =
    Meld
        { error = appError
        , model = appModel
        , tasks = []
        , merges = []
        , commands = []
        }


{-| Merge up model changes and execute commands with latest model as specified
by `withMergeCmds`.
-}
finalize : (Int -> Result x (Meld r x) -> msg) -> Int -> Meld r x -> Model r -> ( Model r, Cmd msg )
finalize toMsg taskCount (Meld { merges, commands }) appModel =
    let
        finalModel =
            merges
                |> List.foldr
                    (\mergeFn accumModel -> mergeFn accumModel)
                    { appModel | meldTasks = appModel.meldTasks - taskCount }
    in
    ( finalModel
    , commands
        |> List.map (\cmdFn -> cmdFn finalModel |> Task.attempt (toMsg 1))
        |> Cmd.batch
    )


{-| Default initial/first `Task` for a sequnce of tasks.
-}
initTask : Meld r x -> Task x (Meld r x)
initTask meld =
    Task.succeed meld


{-| Execute a set of `Task`s added to a `Meld r x` instance in sequence and
update `Model r`'s `meldTasks` counter.
-}
sequence : (Int -> Result x (Meld r x) -> msg) -> Meld r x -> ( Model r, Cmd msg )
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
        , init error nextModel
            |> cmdseq (toMsg taskCount)
        )


{-| Execute a set of `Task`s added to a `Meld r x` instance in any order and
update `Model r`'s `meldTasks` counter.
-}
update : (Int -> Result x (Meld r x) -> msg) -> Meld r x -> ( Model r, Cmd msg )
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
        , init error nextModel
            |> cmds (toMsg 1)
        )


{-| Append a merge function and a list of command task functions to specified `Meld r x`.
-}
withMergeCmds : (Model r -> Model r) -> List (Model r -> Task x (Meld r x)) -> Meld r x -> Meld r x
withMergeCmds mergeFn cmdFns (Meld r) =
    Meld
        { r
            | model = mergeFn r.model
            , merges = mergeFn :: r.merges
            , commands = cmdFns ++ r.commands
        }


{-| Append a list of task functions to specified `Meld r x`.
-}
withTasks : List (Meld r x -> Task x (Meld r x)) -> Meld r x -> Meld r x
withTasks taskFns (Meld r) =
    Meld
        { r | tasks = r.tasks ++ taskFns }

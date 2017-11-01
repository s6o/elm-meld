module Meld
    exposing
        ( Meld
        , Model
        , init
        , sequence
        , update
        , withMergeCmds
        , withTasks
        )

{-| Composable `Task`s, instead of hundreds of Msg pattern match cases.

@docs Meld, Model, init, sequence, update, withMergeCmds, withTasks
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


{-| Create an initial `Meld r x` from specified error and application model types.
-}
init : x -> Model r -> Meld r x
init userError userModel =
    Meld
        { error = userError
        , model = userModel
        , tasks = []
        , merges = []
        , commands = []
        }


{-| Merge up model changes from `sequence` or `update` and execute commands from
the merged up/final model (state).
-}
finalize : (Int -> Result x (Meld r x) -> msg) -> Model r -> Int -> Meld r x -> ( Model r, Cmd msg )
finalize toMsg appModel taskCount (Meld { merges, commands }) =
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


{-| Execute a set of `Task`s added to a `Meld r x` instance in sequence.
-}
sequence : Meld r x -> (Int -> Result x (Meld r x) -> msg) -> ( Model r, Cmd msg )
sequence (Meld { error, model, tasks }) toMsg =
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
        , tasks
            |> List.foldl Task.andThen (init error nextModel |> initTask)
            |> Task.attempt (toMsg taskCount)
        )


{-| Execute a set of `Task`s added to a `Meld r x` instance in any order.
-}
update : Meld r x -> (Int -> Result x (Meld r x) -> msg) -> ( Model r, Cmd msg )
update (Meld { error, model, tasks }) toMsg =
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
        , tasks
            |> List.map
                (\tf ->
                    init error nextModel
                        |> tf
                        |> Task.attempt (toMsg 1)
                )
            |> Cmd.batch
        )


{-| Append a merge function and a list command task functions to specified `Meld r x`.
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

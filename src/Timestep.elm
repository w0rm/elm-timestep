module Timestep exposing
    ( Timestep
    , init
    , advance
    , progress
    , duration
    , steps
    )

{-| Advance a simulation in fixed-size steps, independent of display
refresh rate. Based on Glenn Fiedler’s
[Fix Your Timestep!](https://www.gafferongames.com/post/fix_your_timestep/).

@docs Timestep
@docs init
@docs advance
@docs progress
@docs duration
@docs steps

-}

import Duration exposing (Duration)
import Quantity


{-| The accumulator state. Lives on your model as `timestep`.
-}
type Timestep
    = Timestep
        { duration : Duration
        , elapsed : Duration
        , maxSteps : Int
        , steps : Int
        }


{-| Build a `Timestep`. `duration` is the per-step size (e.g.
`Duration.seconds (1 / 60)` for 60 Hz). `maxSteps` caps how many steps
a single `advance` can run, so a slow frame can’t trigger an
ever-growing catch-up burst; past the cap the simulation goes into
slow-motion. Clamped to at least `1`.

    Timestep.init
        { duration = Duration.seconds (1 / 60)
        , maxSteps = 2
        }

60 Hz with `maxSteps = 2` is a reasonable starting point for most
apps — it absorbs hiccups up to ~33 ms before slowing down.

-}
init : { duration : Duration, maxSteps : Int } -> Timestep
init config =
    Timestep
        { duration = config.duration
        , elapsed = Quantity.zero
        , maxSteps = max 1 config.maxSteps
        , steps = 0
        }


{-| Fraction of a step accumulated since the last fire, in `[0, 1)`.
Blend the previous and current simulation states by this to render
smooth motion at any display rate:

    interpolateFrom
        model.previous
        model.current
        (Timestep.progress model.timestep)

-}
progress : Timestep -> Float
progress (Timestep s) =
    Quantity.ratio s.elapsed s.duration


{-| The per-step duration set in `init`.
-}
duration : Timestep -> Duration
duration (Timestep s) =
    s.duration


{-| How many times the step function was applied in the most recent
`advance` call. Useful for diagnostics.
-}
steps : Timestep -> Int
steps (Timestep s) =
    s.steps


{-| Advance the model by `dt` — runs the step function the right
number of times and stashes the leftover for next frame. The step
function operates on the caller’s whole model, which must carry a
`timestep : Timestep` field:

    Timestep.advance step dt model

-}
advance :
    ({ a | timestep : Timestep } -> { a | timestep : Timestep })
    -> Duration
    -> { a | timestep : Timestep }
    -> { a | timestep : Timestep }
advance stepFn dt model =
    let
        (Timestep s) =
            model.timestep

        next =
            Quantity.plus dt s.elapsed

        wanted =
            floor (Quantity.ratio next s.duration)

        n =
            if wanted - s.maxSteps > 0 then
                s.maxSteps

            else
                wanted

        finalElapsed =
            if wanted - s.maxSteps > 0 then
                Quantity.zero

            else
                Quantity.minus
                    (Quantity.multiplyBy (toFloat wanted) s.duration)
                    next

        steppedModel =
            applyN n
                stepFn
                { model
                    | timestep =
                        Timestep
                            { duration = s.duration
                            , elapsed = Quantity.zero
                            , maxSteps = s.maxSteps
                            , steps = n
                            }
                }
    in
    { steppedModel
        | timestep =
            Timestep
                { duration = s.duration
                , elapsed = finalElapsed
                , maxSteps = s.maxSteps
                , steps = n
                }
    }


applyN : Int -> (a -> a) -> a -> a
applyN n f x =
    if n <= 0 then
        x

    else
        applyN (n - 1) f (f x)

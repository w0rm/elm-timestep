# elm-timestep

Advance a simulation in fixed-size steps, independent of display
refresh rate. Based on Glenn Fiedler's
[Fix Your Timestep!](https://www.gafferongames.com/post/fix_your_timestep/).

`Browser.Events.onAnimationFrameDelta` fires at the display rate —
60 Hz, 120 Hz, irregular under load. Integrating motion by the raw
Δt makes the simulation behave differently per machine. `advance`
runs the step function as many times as the elapsed Δt covers and
saves the leftover, so the simulation runs at the rate you chose
regardless of what the browser is doing.

```elm
import Duration exposing (Duration)
import Length exposing (Length)
import Quantity
import Speed exposing (Speed)
import Timestep exposing (Timestep)


type alias Model =
    { position : Length
    , velocity : Speed
    , timestep : Timestep
    }


init : Model
init =
    { position = Quantity.zero
    , velocity = Speed.metersPerSecond 1
    , timestep =
        Timestep.init
            { duration = Duration.seconds (1 / 60)
            , maxSteps = 2
            }
    }


update : Duration -> Model -> Model
update dt model =
    Timestep.advance step dt model


step : Model -> Model
step model =
    let
        traveled =
            model.velocity |> Quantity.for (Timestep.duration model.timestep)
    in
    { model | position = model.position |> Quantity.plus traveled }
```

## Spiral of death

A slow frame produces a big Δt, which produces a big catch-up burst,
which makes the next frame even slower. `maxSteps` caps how many
steps a single `advance` can run, so the catch-up can’t snowball.
Past the cap, the simulation goes into slow motion instead.

- `maxSteps = 1` — never catch up; hiccups cost simulated time.
- `maxSteps = 2` — absorb one missed step. A good default.

## Interpolation

When the simulation rate doesn’t divide the display rate, steps fire
unevenly and the scene stutters. To smooth it, keep the previous
position alongside the current one and blend by `Timestep.progress`
(the fraction of a step accumulated, in `[0, 1)`):

```elm
type alias Model =
    { prevPosition : Length
    , position : Length
    , velocity : Speed
    , timestep : Timestep
    }


step : Model -> Model
step model =
    let
        traveled =
            model.velocity |> Quantity.for (Timestep.duration model.timestep)
    in
    { model
        | prevPosition = model.position
        , position = model.position |> Quantity.plus traveled
    }


rendered : Model -> Length
rendered model =
    Quantity.interpolateFrom
        model.prevPosition
        model.position
        (Timestep.progress model.timestep)
```

This renders one step behind wall-clock — the trade for motion that’s
smooth at any display rate without ever extrapolating past a computed
state.

## Examples

- Bouncing ball ([source](https://github.com/w0rm/elm-timestep/tree/main/examples/src/BouncingBall.elm), [demo](https://unsoundscapes.com/elm-timestep/examples/bouncing-ball/)) — naïve variable-Δt vs fixed timestep, with a stall button.

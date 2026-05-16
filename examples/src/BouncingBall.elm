module BouncingBall exposing (main)

{-| Bouncing ball demo for elm-timestep.

Two balls share an initial position and velocity. The red one advances
by the raw `dt` of every animation frame — the naïve approach. The
blue one runs through `Timestep.advance` at a fixed simulation rate.

A stall button injects a big synthetic frame so you can see the
spiral-of-death case the package exists to prevent: the red ball
teleports (and may tunnel through walls); the blue ball moves at most
`maxSteps × stepDuration` worth and then keeps going at its real pace.

Drop the step rate to 5–10 Hz with interpolation off to see the
blue ball stutter — that's snap rendering. Turn interpolation back
on for the smooth version.

-}

import Browser
import Browser.Events
import Duration exposing (Duration)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Length exposing (Length)
import Quantity
import Speed exposing (Speed)
import Svg
import Svg.Attributes as SvgAttr
import Timestep exposing (Timestep)



-- 1 m == 1 px in the SVG.


fieldWidth : Length
fieldWidth =
    Length.meters 480


fieldHeight : Length
fieldHeight =
    Length.meters 260


ballRadius : Length
ballRadius =
    Length.meters 16


initialX : Length
initialX =
    Length.meters 80


initialY : Length
initialY =
    Length.meters 60


initialVx : Speed
initialVx =
    Speed.metersPerSecond 240


initialVy : Speed
initialVy =
    Speed.metersPerSecond 185


type alias Body =
    { x : Length
    , y : Length
    , prevX : Length
    , prevY : Length
    , vx : Speed
    , vy : Speed
    , timestep : Timestep
    }


type alias Naive =
    { x : Length
    , y : Length
    , vx : Speed
    , vy : Speed
    }


type alias Model =
    { fixed : Body
    , naive : Naive
    , stepHz : Int
    , maxSteps : Int
    , pendingStall : Duration
    , lastSubsteps : Int
    , lastDt : Duration
    , interpolate : Bool
    }


type Msg
    = Frame Float
    | SetHz Int
    | SetMaxSteps Int
    | Stall Float
    | ToggleInterpolate
    | Reset


buildTimestep : Int -> Int -> Timestep
buildTimestep hz maxSteps =
    Timestep.init
        { duration = Duration.seconds (1 / toFloat hz)
        , maxSteps = maxSteps
        }


initialBody : Int -> Int -> Body
initialBody hz maxSteps =
    { x = initialX
    , y = initialY
    , prevX = initialX
    , prevY = initialY
    , vx = initialVx
    , vy = initialVy
    , timestep = buildTimestep hz maxSteps
    }


initialNaive : Naive
initialNaive =
    { x = initialX
    , y = initialY
    , vx = initialVx
    , vy = initialVy
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        hz =
            10

        maxSteps =
            2
    in
    ( { fixed = initialBody hz maxSteps
      , naive = initialNaive
      , stepHz = hz
      , maxSteps = maxSteps
      , pendingStall = Quantity.zero
      , lastSubsteps = 0
      , lastDt = Quantity.zero
      , interpolate = True
      }
    , Cmd.none
    )



-- The fixed-step update from the README, almost verbatim.


step : Body -> Body
step body =
    let
        dt =
            Timestep.duration body.timestep

        ( newX, newVx ) =
            bounce body.x body.vx dt ballRadius (fieldWidth |> Quantity.minus ballRadius)

        ( newY, newVy ) =
            bounce body.y body.vy dt ballRadius (fieldHeight |> Quantity.minus ballRadius)
    in
    { body
        | prevX = body.x
        , prevY = body.y
        , x = newX
        , y = newY
        , vx = newVx
        , vy = newVy
    }



-- The naïve update: just integrate by the frame delta.


stepNaive : Duration -> Naive -> Naive
stepNaive dt naive =
    let
        ( newX, newVx ) =
            bounce naive.x naive.vx dt ballRadius (fieldWidth |> Quantity.minus ballRadius)

        ( newY, newVy ) =
            bounce naive.y naive.vy dt ballRadius (fieldHeight |> Quantity.minus ballRadius)
    in
    { naive | x = newX, y = newY, vx = newVx, vy = newVy }


bounce :
    Length
    -> Speed
    -> Duration
    -> Length
    -> Length
    -> ( Length, Speed )
bounce pos vel dt low high =
    let
        next =
            pos |> Quantity.plus (vel |> Quantity.for dt)
    in
    if next |> Quantity.lessThan low then
        ( Quantity.multiplyBy 2 low |> Quantity.minus next
        , Quantity.negate vel
        )

    else if next |> Quantity.greaterThan high then
        ( Quantity.multiplyBy 2 high |> Quantity.minus next
        , Quantity.negate vel
        )

    else
        ( next, vel )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame ms ->
            let
                dt =
                    Duration.milliseconds ms
                        |> Quantity.plus model.pendingStall

                fixed =
                    Timestep.advance step dt model.fixed

                naive =
                    stepNaive dt model.naive
            in
            ( { model
                | fixed = fixed
                , naive = naive
                , pendingStall = Quantity.zero
                , lastSubsteps = Timestep.steps fixed.timestep
                , lastDt = dt
              }
            , Cmd.none
            )

        SetHz hz ->
            ( reset { model | stepHz = hz }, Cmd.none )

        SetMaxSteps n ->
            ( reset { model | maxSteps = n }, Cmd.none )

        Stall ms ->
            ( { model | pendingStall = Duration.milliseconds ms }, Cmd.none )

        ToggleInterpolate ->
            ( { model | interpolate = not model.interpolate }, Cmd.none )

        Reset ->
            ( reset model, Cmd.none )


reset : Model -> Model
reset model =
    { model
        | fixed = initialBody model.stepHz model.maxSteps
        , naive = initialNaive
        , pendingStall = Quantity.zero
        , lastSubsteps = 0
        , lastDt = Quantity.zero
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta Frame


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.style "font-family" "system-ui, -apple-system, sans-serif"
        , Attr.style "max-width" "560px"
        , Attr.style "margin" "24px auto"
        , Attr.style "padding" "0 16px"
        , Attr.style "color" "#222"
        , Attr.style "line-height" "1.5"
        ]
        [ Html.h1
            [ Attr.style "margin-bottom" "4px" ]
            [ Html.text "elm-timestep — bouncing ball" ]
        , Html.p
            [ Attr.style "color" "#666"
            , Attr.style "margin-top" "0"
            ]
            [ Html.text "Two simulations of the same ball, same starting state." ]
        , Html.ul
            [ Attr.style "color" "#444"
            , Attr.style "padding-left" "20px"
            ]
            [ Html.li []
                [ coloured "#c0392b" "Naïve"
                , Html.text " — integrates by raw frame Δt."
                ]
            , Html.li []
                [ coloured "#2980b9" "Fixed"
                , Html.text " — uses "
                , code "Timestep.advance"
                , Html.text "."
                ]
            ]
        , viewScene model
        , viewControls model
        , viewStats model
        , viewHints
        ]


coloured : String -> String -> Html msg
coloured colour label =
    Html.strong [ Attr.style "color" colour ] [ Html.text label ]


code : String -> Html msg
code text =
    Html.code
        [ Attr.style "background" "#f0f0f0"
        , Attr.style "padding" "0 4px"
        , Attr.style "border-radius" "3px"
        , Attr.style "font-size" "0.9em"
        ]
        [ Html.text text ]


renderL : Length -> String
renderL l =
    String.fromFloat (Length.inMeters l)


viewScene : Model -> Html Msg
viewScene model =
    let
        ( fixedX, fixedY ) =
            if model.interpolate then
                ( Quantity.interpolateFrom
                    model.fixed.prevX
                    model.fixed.x
                    (Timestep.progress model.fixed.timestep)
                , Quantity.interpolateFrom
                    model.fixed.prevY
                    model.fixed.y
                    (Timestep.progress model.fixed.timestep)
                )

            else
                ( model.fixed.x, model.fixed.y )
    in
    Svg.svg
        [ SvgAttr.width (renderL fieldWidth)
        , SvgAttr.height (renderL fieldHeight)
        , SvgAttr.viewBox ("0 0 " ++ renderL fieldWidth ++ " " ++ renderL fieldHeight)
        , Attr.style "display" "block"
        , Attr.style "background" "#fafafa"
        , Attr.style "border" "1px solid #ddd"
        , Attr.style "border-radius" "4px"
        , Attr.style "margin-top" "12px"
        ]
        [ Svg.circle
            [ SvgAttr.cx (renderL model.naive.x)
            , SvgAttr.cy (renderL model.naive.y)
            , SvgAttr.r (renderL ballRadius)
            , SvgAttr.fill "#c0392b"
            , SvgAttr.opacity "0.7"
            ]
            []
        , Svg.circle
            [ SvgAttr.cx (renderL fixedX)
            , SvgAttr.cy (renderL fixedY)
            , SvgAttr.r (renderL ballRadius)
            , SvgAttr.fill "#2980b9"
            , SvgAttr.opacity "0.7"
            ]
            []
        ]


viewControls : Model -> Html Msg
viewControls model =
    Html.div
        [ Attr.style "display" "grid"
        , Attr.style "grid-template-columns" "1fr 1fr"
        , Attr.style "gap" "12px 24px"
        , Attr.style "margin-top" "16px"
        ]
        [ slider "Step rate"
            (String.fromInt model.stepHz ++ " Hz")
            { min = 2, max = 120, value = model.stepHz, msg = SetHz }
        , slider "maxSteps"
            (String.fromInt model.maxSteps)
            { min = 1, max = 8, value = model.maxSteps, msg = SetMaxSteps }
        , Html.label
            [ Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "gap" "8px"
            ]
            [ Html.input
                [ Attr.type_ "checkbox"
                , Attr.checked model.interpolate
                , Events.onClick ToggleInterpolate
                ]
                []
            , Html.span [] [ Html.text "Interpolate fixed ball" ]
            ]
        , Html.div
            [ Attr.style "display" "flex"
            , Attr.style "gap" "6px"
            , Attr.style "flex-wrap" "wrap"
            ]
            [ button (Stall 100) "Stall 100 ms"
            , button (Stall 500) "Stall 500 ms"
            , button (Stall 2000) "Stall 2 s"
            , button Reset "Reset"
            ]
        ]


slider :
    String
    -> String
    -> { min : Int, max : Int, value : Int, msg : Int -> Msg }
    -> Html Msg
slider label readout config =
    Html.label
        [ Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "gap" "4px"
        , Attr.style "font-size" "13px"
        ]
        [ Html.span []
            [ Html.text label
            , Html.span [ Attr.style "color" "#888" ]
                [ Html.text (" · " ++ readout) ]
            ]
        , Html.input
            [ Attr.type_ "range"
            , Attr.min (String.fromInt config.min)
            , Attr.max (String.fromInt config.max)
            , Attr.step "1"
            , Attr.value (String.fromInt config.value)
            , Events.onInput
                (\s ->
                    config.msg
                        (String.toInt s |> Maybe.withDefault config.value)
                )
            ]
            []
        ]


button : Msg -> String -> Html Msg
button msg label =
    Html.button
        [ Events.onClick msg
        , Attr.type_ "button"
        , Attr.style "padding" "6px 10px"
        , Attr.style "font-size" "13px"
        , Attr.style "border" "1px solid #ccc"
        , Attr.style "background" "#fff"
        , Attr.style "border-radius" "4px"
        , Attr.style "cursor" "pointer"
        ]
        [ Html.text label ]


viewStats : Model -> Html msg
viewStats model =
    Html.div
        [ Attr.style "margin-top" "16px"
        , Attr.style "font-family" "ui-monospace, SFMono-Regular, Menlo, monospace"
        , Attr.style "font-size" "12px"
        , Attr.style "color" "#666"
        ]
        [ Html.text
            ("Δt = "
                ++ String.fromFloat (round100 (Duration.inMilliseconds model.lastDt))
                ++ " ms  ·  substeps this frame = "
                ++ String.fromInt model.lastSubsteps
                ++ "  ·  progress = "
                ++ String.fromFloat (round100 (Timestep.progress model.fixed.timestep))
            )
        ]


viewHints : Html msg
viewHints =
    Html.details
        [ Attr.style "margin-top" "20px"
        , Attr.style "color" "#444"
        ]
        [ Html.summary [] [ Html.text "Things to try" ]
        , Html.ul []
            [ Html.li []
                [ Html.text "Click "
                , code "Stall 2 s"
                , Html.text ". The red ball jumps the full 2 s of motion — often "
                , Html.text "passing through a wall — while the blue ball advances by only "
                , code "maxSteps × stepDuration"
                , Html.text " and resumes at its real pace."
                ]
            , Html.li []
                [ Html.text "Drop step rate to 5 Hz and turn off interpolation: "
                , Html.text "the blue ball snaps in visible chunks. Turn it back on to smooth it."
                ]
            , Html.li []
                [ Html.text "Push step rate above the display rate (e.g. 120 Hz on a 60 Hz "
                , Html.text "screen): "
                , code "substeps"
                , Html.text " climbs above 1 each frame."
                ]
            ]
        ]


round100 : Float -> Float
round100 f =
    toFloat (round (f * 100)) / 100


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

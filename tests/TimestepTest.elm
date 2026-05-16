module TimestepTest exposing (suite)

import Duration exposing (Duration)
import Expect
import Fuzz exposing (Fuzzer)
import Quantity
import Test exposing (Test, describe, fuzz2, test)
import Timestep exposing (Timestep)


step60 : Duration
step60 =
    Duration.seconds (1 / 60)


step120 : Duration
step120 =
    Duration.seconds (1 / 120)


type alias TestModel =
    { counter : Int
    , pattern : List Int
    , timestep : Timestep
    }


emptyModel : Timestep -> TestModel
emptyModel ts =
    { counter = 0
    , pattern = []
    , timestep = ts
    }


record : TestModel -> TestModel
record m =
    { m | counter = m.counter + 1 }


{-| Helper to run N frames at a constant dt.
-}
run : Duration -> Int -> Timestep -> { substeps : Int, pattern : List Int }
run dt frames timestep =
    runHelp dt frames timestep 0 []


runHelp : Duration -> Int -> Timestep -> Int -> List Int -> { substeps : Int, pattern : List Int }
runHelp dt remaining timestep total pattern =
    if remaining <= 0 then
        { substeps = total, pattern = List.reverse pattern }

    else
        let
            nextModel =
                Timestep.advance identity dt { counter = 0, pattern = [], timestep = timestep }

            n =
                Timestep.steps nextModel.timestep
        in
        runHelp dt (remaining - 1) nextModel.timestep (total + n) (n :: pattern)


step60Timestep : Timestep
step60Timestep =
    Timestep.init { duration = step60, maxSteps = 1 }


type alias Config =
    { duration : Duration, maxSteps : Int }


fuzzConfig : Fuzzer Config
fuzzConfig =
    Fuzz.map2 Config
        (Fuzz.map Duration.milliseconds (Fuzz.floatRange 1 50))
        (Fuzz.intRange 1 10)


fuzzDts : Fuzzer (List Duration)
fuzzDts =
    Fuzz.listOfLength 200 (Fuzz.map Duration.milliseconds (Fuzz.floatRange 0 200))


foldFrames : Config -> List Duration -> { perFrame : List Int, timestep : Timestep }
foldFrames config dts =
    let
        initial =
            Timestep.init
                { duration = config.duration
                , maxSteps = config.maxSteps
                }

        ( finalTs, framesRev ) =
            List.foldl
                (\dt ( ts, acc ) ->
                    let
                        nextModel =
                            Timestep.advance identity dt { timestep = ts }
                    in
                    ( nextModel.timestep, Timestep.steps nextModel.timestep :: acc )
                )
                ( initial, [] )
                dts
    in
    { perFrame = List.reverse framesRev, timestep = finalTs }


simulatedTime : Config -> { perFrame : List Int, timestep : Timestep } -> Duration
simulatedTime config result =
    Quantity.plus
        (Quantity.multiplyBy (toFloat (List.sum result.perFrame)) config.duration)
        (Quantity.multiplyBy (Timestep.progress result.timestep) config.duration)


suite : Test
suite =
    describe "Timestep"
        [ describe "60Hz target, maxSteps = 1"
            [ test "60Hz display fires every frame" <|
                \_ ->
                    (run step60 600 step60Timestep).substeps
                        |> Expect.equal 600
            , test "120Hz display fires every other frame" <|
                \_ ->
                    (run step120 1200 step60Timestep).substeps
                        |> Expect.equal 600
            , test "dt = 2·step every frame: 1 substep per frame, no state growth" <|
                \_ ->
                    (run (Quantity.multiplyBy 2 step60) 100 step60Timestep).substeps
                        |> Expect.equal 100
            , test "a 1s freeze advances simulation by one step, no catch-up burst" <|
                \_ ->
                    let
                        slow =
                            (Timestep.advance identity (Duration.seconds 1) (emptyModel step60Timestep)).timestep
                    in
                    (run step120 100 slow).substeps
                        |> Expect.equal 50
            ]
        , describe "step function calls"
            [ test "0 substeps: step function not called" <|
                \_ ->
                    let
                        result =
                            Timestep.advance record (Quantity.multiplyBy 0.5 step60) (emptyModel step60Timestep)
                    in
                    ( result.counter, Timestep.steps result.timestep )
                        |> Expect.equal ( 0, 0 )
            , test "1 substep: step function called once" <|
                \_ ->
                    let
                        result =
                            Timestep.advance record step60 (emptyModel step60Timestep)
                    in
                    ( result.counter, Timestep.steps result.timestep )
                        |> Expect.equal ( 1, 1 )
            , test "2 substeps with maxSteps=2: step function called twice" <|
                \_ ->
                    let
                        timestep =
                            Timestep.init { duration = step60, maxSteps = 2 }

                        result =
                            Timestep.advance record (Quantity.multiplyBy 2 step60) (emptyModel timestep)
                    in
                    ( result.counter, Timestep.steps result.timestep )
                        |> Expect.equal ( 2, 2 )
            ]
        , describe "120Hz target, maxSteps = 2"
            [ let
                timestep =
                    Timestep.init { duration = step120, maxSteps = 2 }
              in
              describe "shape"
                [ test "120Hz display runs 1 substep per frame" <|
                    \_ ->
                        (run step120 600 timestep).substeps
                            |> Expect.equal 600
                , test "60Hz display fires exactly 2 each frame, no skips" <|
                    \_ ->
                        (run step60 12 timestep).pattern
                            |> Expect.equal (List.repeat 12 2)
                ]
            ]
        , describe "invariants under any config and dt sequence"
            [ fuzz2 fuzzConfig fuzzDts "progress always stays in [0, 1)" <|
                \config dts ->
                    let
                        p =
                            Timestep.progress (foldFrames config dts).timestep
                    in
                    ( p >= 0, p < 1 )
                        |> Expect.equal ( True, True )
            , fuzz2 fuzzConfig fuzzDts "substeps per frame never exceeds maxSteps" <|
                \config dts ->
                    List.any (\n -> n > config.maxSteps || n < 0) (foldFrames config dts).perFrame
                        |> Expect.equal False
            , fuzz2 fuzzConfig fuzzDts "simulated time never gets ahead of wall time" <|
                \config dts ->
                    let
                        wall =
                            Quantity.sum dts

                        sim =
                            simulatedTime config (foldFrames config dts)

                        tolerance =
                            Duration.milliseconds 0.001
                    in
                    sim
                        |> Quantity.lessThanOrEqualTo (Quantity.plus wall tolerance)
                        |> Expect.equal True
            , fuzz2 fuzzConfig fuzzDts "simulated tracks wall within one step when no frame exceeds maxSteps × step" <|
                \config dts ->
                    let
                        budget =
                            Quantity.multiplyBy (toFloat config.maxSteps) config.duration
                    in
                    if List.any (Quantity.greaterThan budget) dts then
                        Expect.pass

                    else
                        let
                            wall =
                                Quantity.sum dts

                            sim =
                                simulatedTime config (foldFrames config dts)
                        in
                        Quantity.abs (Quantity.minus sim wall)
                            |> Quantity.lessThanOrEqualTo config.duration
                            |> Expect.equal True
            ]
        ]

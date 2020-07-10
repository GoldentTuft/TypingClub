module CountDown exposing
    ( State(..)
    , Timer
    , getSecond
    , getState
    , init
    , setStart
    , setStop
    , tickToZero
    , toggleStart
    , treatSubscription
    )

import Time


type Timer
    = Timer
        { start : Bool
        , initialTime : Int
        , time : Int
        , interval : Int
        }


type State
    = Ready
    | Tick
    | Stop
    | Zero
    | Error


init : Bool -> Int -> Int -> Timer
init start initialTime interval =
    -- 引数のチェックはどうするか?
    -- 使う側はinit False 3000 1000とか、ハードコーディングだと思うし。
    Timer
        { start = start
        , initialTime = initialTime
        , time = initialTime
        , interval = interval
        }


reset : Timer -> Timer
reset (Timer timer) =
    Timer
        { timer
            | time = timer.initialTime
        }


getState : Timer -> State
getState (Timer timer) =
    case timer.start of
        True ->
            if timer.time <= 0 then
                Zero

            else
                Tick

        False ->
            if timer.time <= 0 then
                Zero

            else if 0 < timer.time && timer.time < timer.initialTime then
                Stop

            else if timer.time == timer.initialTime then
                Ready

            else
                Error


setStart : Timer -> Timer
setStart (Timer timer) =
    Timer { timer | start = True }


setStop : Timer -> Timer
setStop (Timer timer) =
    Timer { timer | start = False }


toggleStart : Timer -> Timer
toggleStart (Timer timer) =
    Timer { timer | start = not timer.start }


getSecond : Timer -> Int
getSecond (Timer timer) =
    toFloat timer.time / 1000 |> round


tickToZero : Timer -> Timer
tickToZero (Timer timer) =
    Timer { timer | time = 0, start = False }


tick : Timer -> Timer
tick (Timer timer) =
    let
        newTime =
            if (timer.time - timer.interval) < 0 then
                0

            else
                timer.time - timer.interval

        newStart =
            if newTime == 0 then
                False

            else
                True
    in
    Timer { timer | time = newTime, start = newStart }


treatSubscription : Timer -> (Timer -> msg) -> Sub msg
treatSubscription (Timer timer) userMsg =
    if List.member (getState (Timer timer)) [ Zero, Stop, Ready ] then
        Sub.none

    else
        let
            subTick : Timer -> Time.Posix -> Timer
            subTick subTimer time =
                tick subTimer

            sub : Sub Timer
            sub =
                Time.every (toFloat timer.interval) (subTick (Timer timer))
        in
        Sub.map userMsg sub

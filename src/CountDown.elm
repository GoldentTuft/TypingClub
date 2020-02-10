module CountDown exposing (Timer, getSecond, init, isZero, treatSubscription)

import Time


type Timer
    = Timer
        { start : Bool
        , initialTime : Int
        , time : Int
        , interval : Int
        }


init : Bool -> Int -> Int -> Timer
init start initialTime interval =
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


isStart : Timer -> Bool
isStart (Timer timer) =
    timer.start == True


isZero : Timer -> Bool
isZero (Timer timer) =
    timer.time <= 0


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


tick : Timer -> Timer
tick (Timer timer) =
    let
        newTime =
            if (timer.time - timer.interval) < 0 then
                0

            else
                timer.time - timer.interval
    in
    Timer { timer | time = newTime }


treatSubscription : Timer -> (Timer -> msg) -> Sub msg
treatSubscription (Timer timer) userMsg =
    if isZero (Timer timer) || isStart (Timer timer) == False then
        Sub.none

    else
        let
            subTick subTimer time =
                tick subTimer

            sub =
                Time.every (toFloat timer.interval) (subTick (Timer timer))
        in
        Sub.map userMsg sub

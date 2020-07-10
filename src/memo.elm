let
    newT = Typing.typeTo key typingData
    newS = newT |> Typing.toState of
in
case model.typingData |> Typing.toState of
    Waiting ->
        case newS of
            Waiting ->
                -- Waitingにはならないのに。
                (model, Cmd.none)
            Typing ->
                -- 正入力
                ({model | typingData = newT}, startTyping)
            Miss ->
                -- TypingLongWordではなにもしない
                -- スタートしてミスにしてもいい
                (model, Cmd.none)
            Finish ->
                -- 正入力かつ終了
                ({model | typingData = newT}, finishTyping)
    Typing
        case newS of
            Waiting ->
                -- Waitingにはならないのに。
                (model, Cmd.none)
            Typing ->
                -- 正入力
                ({model | typingData = newT}, Cmd.none)
            Miss ->
                -- 誤入力
                ({model | typingData = newT}, Cmd.none)
            Finish ->
                -- 正入力かつ終了
                ({model | typingData = newT}, finishTyping)
    Miss ->
        case newS of
            Waiting ->
                -- これも
                (model, Cmd.none)
            Typing ->
                -- 正入力
                ({model | typingData = newT}, Cmd.none)
            Miss ->
                -- 誤入力
                ({model | typingData = newT}, Cmd.none)
            Finish ->
                -- 正入力かつ終了
                ({model | typingData = newT}, finishTyping)
    Finish ->
        -- 入力無効
        (model, Cmd.none)

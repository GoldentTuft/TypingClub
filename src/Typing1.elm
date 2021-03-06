module Typing1 exposing (Data, Rule, State, newData, nextData, romanTable, typeTo)


type alias Rule =
    { input : String
    , output : String
    }


romanTable : List Rule
romanTable =
    [ Rule "-" "ー"
    , Rule "~" "〜"
    , Rule "." "。"
    , Rule "," "、"
    , Rule "[" "「"
    , Rule "]" "」"
    , Rule "va" "ゔぁ"
    , Rule "vi" "ゔぃ"
    , Rule "vu" "ゔ"
    , Rule "ve" "ゔぇ"
    , Rule "vo" "ゔぉ"
    , Rule "vya" "ゔゃ"
    , Rule "vyi" "ゔぃ"
    , Rule "vyu" "ゔゅ"
    , Rule "vye" "ゔぇ"
    , Rule "vyo" "ゔょ"
    , Rule "kya" "きゃ"
    , Rule "kyi" "きぃ"
    , Rule "kyu" "きゅ"
    , Rule "kye" "きぇ"
    , Rule "kyo" "きょ"
    , Rule "gya" "ぎゃ"
    , Rule "gyi" "ぎぃ"
    , Rule "gyu" "ぎゅ"
    , Rule "gye" "ぎぇ"
    , Rule "gyo" "ぎょ"
    , Rule "sya" "しゃ"
    , Rule "syi" "しぃ"
    , Rule "syu" "しゅ"
    , Rule "sye" "しぇ"
    , Rule "syo" "しょ"
    , Rule "sha" "しゃ"
    , Rule "shi" "し"
    , Rule "shu" "しゅ"
    , Rule "she" "しぇ"
    , Rule "sho" "しょ"
    , Rule "zya" "じゃ"
    , Rule "zyi" "じぃ"
    , Rule "zyu" "じゅ"
    , Rule "zye" "じぇ"
    , Rule "zyo" "じょ"
    , Rule "tya" "ちゃ"
    , Rule "tyi" "ちぃ"
    , Rule "tyu" "ちゅ"
    , Rule "tye" "ちぇ"
    , Rule "tyo" "ちょ"
    , Rule "cha" "ちゃ"
    , Rule "chi" "ち"
    , Rule "chu" "ちゅ"
    , Rule "che" "ちぇ"
    , Rule "cho" "ちょ"
    , Rule "cya" "ちゃ"
    , Rule "cyi" "ちぃ"
    , Rule "cyu" "ちゅ"
    , Rule "cye" "ちぇ"
    , Rule "cyo" "ちょ"
    , Rule "dya" "ぢゃ"
    , Rule "dyi" "ぢぃ"
    , Rule "dyu" "ぢゅ"
    , Rule "dye" "ぢぇ"
    , Rule "dyo" "ぢょ"
    , Rule "tsa" "つぁ"
    , Rule "tsi" "つぃ"
    , Rule "tse" "つぇ"
    , Rule "tso" "つぉ"
    , Rule "tha" "てゃ"
    , Rule "thi" "てぃ"
    , Rule "thu" "てゅ"
    , Rule "the" "てぇ"
    , Rule "tho" "てょ"
    , Rule "dha" "でゃ"
    , Rule "dhi" "でぃ"
    , Rule "dhu" "でゅ"
    , Rule "dhe" "でぇ"
    , Rule "dho" "でょ"
    , Rule "twa" "とぁ"
    , Rule "twi" "とぃ"
    , Rule "twu" "とぅ"
    , Rule "twe" "とぇ"
    , Rule "two" "とぉ"
    , Rule "dwa" "どぁ"
    , Rule "dwi" "どぃ"
    , Rule "dwu" "どぅ"
    , Rule "dwe" "どぇ"
    , Rule "dwo" "どぉ"
    , Rule "nya" "にゃ"
    , Rule "nyi" "にぃ"
    , Rule "nyu" "にゅ"
    , Rule "nye" "にぇ"
    , Rule "nyo" "にょ"
    , Rule "hya" "ひゃ"
    , Rule "hyi" "ひぃ"
    , Rule "hyu" "ひゅ"
    , Rule "hye" "ひぇ"
    , Rule "hyo" "ひょ"
    , Rule "bya" "びゃ"
    , Rule "byi" "びぃ"
    , Rule "byu" "びゅ"
    , Rule "bye" "びぇ"
    , Rule "byo" "びょ"
    , Rule "pya" "ぴゃ"
    , Rule "pyi" "ぴぃ"
    , Rule "pyu" "ぴゅ"
    , Rule "pye" "ぴぇ"
    , Rule "pyo" "ぴょ"
    , Rule "fa" "ふぁ"
    , Rule "fi" "ふぃ"
    , Rule "fu" "ふ"
    , Rule "fe" "ふぇ"
    , Rule "fo" "ふぉ"
    , Rule "fya" "ふゃ"
    , Rule "fyu" "ふゅ"
    , Rule "fyo" "ふょ"
    , Rule "mya" "みゃ"
    , Rule "myi" "みぃ"
    , Rule "myu" "みゅ"
    , Rule "mye" "みぇ"
    , Rule "myo" "みょ"
    , Rule "rya" "りゃ"
    , Rule "ryi" "りぃ"
    , Rule "ryu" "りゅ"
    , Rule "rye" "りぇ"
    , Rule "ryo" "りょ"
    , Rule "nn" "ん"
    , Rule "n" "ん"
    , Rule "xn" "ん"
    , Rule "a" "あ"
    , Rule "i" "い"
    , Rule "u" "う"
    , Rule "wu" "う"
    , Rule "e" "え"
    , Rule "o" "お"
    , Rule "xa" "ぁ"
    , Rule "xi" "ぃ"
    , Rule "xu" "ぅ"
    , Rule "xe" "ぇ"
    , Rule "xo" "ぉ"
    , Rule "la" "ぁ"
    , Rule "li" "ぃ"
    , Rule "lu" "ぅ"
    , Rule "le" "ぇ"
    , Rule "lo" "ぉ"
    , Rule "lyi" "ぃ"
    , Rule "xyi" "ぃ"
    , Rule "lye" "ぇ"
    , Rule "xye" "ぇ"
    , Rule "ye" "いぇ"
    , Rule "ka" "か"
    , Rule "ki" "き"
    , Rule "ku" "く"
    , Rule "ke" "け"
    , Rule "ko" "こ"
    , Rule "xka" "ヵ"
    , Rule "xke" "ヶ"
    , Rule "lka" "ヵ"
    , Rule "lke" "ヶ"
    , Rule "ga" "が"
    , Rule "gi" "ぎ"
    , Rule "gu" "ぐ"
    , Rule "ge" "げ"
    , Rule "go" "ご"
    , Rule "sa" "さ"
    , Rule "si" "し"
    , Rule "su" "す"
    , Rule "se" "せ"
    , Rule "so" "そ"
    , Rule "ca" "か"
    , Rule "ci" "し"
    , Rule "cu" "く"
    , Rule "ce" "せ"
    , Rule "co" "こ"
    , Rule "qa" "くぁ"
    , Rule "qi" "くぃ"
    , Rule "qu" "く"
    , Rule "qe" "くぇ"
    , Rule "qo" "くぉ"
    , Rule "kwa" "くぁ"
    , Rule "gwa" "ぐぁ"
    , Rule "gwi" "ぐぃ"
    , Rule "gwu" "ぐぅ"
    , Rule "gwe" "ぐぇ"
    , Rule "gwo" "ぐぉ"
    , Rule "za" "ざ"
    , Rule "zi" "じ"
    , Rule "zu" "ず"
    , Rule "ze" "ぜ"
    , Rule "zo" "ぞ"
    , Rule "ja" "じゃ"
    , Rule "ji" "じ"
    , Rule "ju" "じゅ"
    , Rule "je" "じぇ"
    , Rule "jo" "じょ"
    , Rule "jya" "じゃ"
    , Rule "jyi" "じぃ"
    , Rule "jyu" "じゅ"
    , Rule "jye" "じぇ"
    , Rule "jyo" "じょ"
    , Rule "ta" "た"
    , Rule "ti" "ち"
    , Rule "tu" "つ"
    , Rule "tsu" "つ"
    , Rule "te" "て"
    , Rule "to" "と"
    , Rule "da" "だ"
    , Rule "di" "ぢ"
    , Rule "du" "づ"
    , Rule "de" "で"
    , Rule "do" "ど"
    , Rule "xtu" "っ"
    , Rule "xtsu" "っ"
    , Rule "ltu" "っ"
    , Rule "ltsu" "っ"
    , Rule "na" "な"
    , Rule "ni" "に"
    , Rule "nu" "ぬ"
    , Rule "ne" "ね"
    , Rule "no" "の"
    , Rule "ha" "は"
    , Rule "hi" "ひ"
    , Rule "hu" "ふ"
    , Rule "fu" "ふ"
    , Rule "he" "へ"
    , Rule "ho" "ほ"
    , Rule "ba" "ば"
    , Rule "bi" "び"
    , Rule "bu" "ぶ"
    , Rule "be" "べ"
    , Rule "bo" "ぼ"
    , Rule "pa" "ぱ"
    , Rule "pi" "ぴ"
    , Rule "pu" "ぷ"
    , Rule "pe" "ぺ"
    , Rule "po" "ぽ"
    , Rule "ma" "ま"
    , Rule "mi" "み"
    , Rule "mu" "む"
    , Rule "me" "め"
    , Rule "mo" "も"
    , Rule "xya" "ゃ"
    , Rule "lya" "ゃ"
    , Rule "ya" "や"
    , Rule "wyi" "ゐ"
    , Rule "xyu" "ゅ"
    , Rule "lyu" "ゅ"
    , Rule "yu" "ゆ"
    , Rule "wye" "ゑ"
    , Rule "xyo" "ょ"
    , Rule "lyo" "ょ"
    , Rule "yo" "よ"
    , Rule "ra" "ら"
    , Rule "ri" "り"
    , Rule "ru" "る"
    , Rule "re" "れ"
    , Rule "ro" "ろ"
    , Rule "xwa" "ゎ"
    , Rule "lwa" "ゎ"
    , Rule "wa" "わ"
    , Rule "wi" "うぃ"
    , Rule "we" "うぇ"
    , Rule "wo" "を"
    , Rule "wha" "うぁ"
    , Rule "whi" "うぃ"
    , Rule "whu" "う"
    , Rule "whe" "うぇ"
    , Rule "who" "うぉ"
    , Rule "qqa" "っくぁ"
    , Rule "qqi" "っくぃ"
    , Rule "qqu" "っく"
    , Rule "qqe" "っくぇ"
    , Rule "qqo" "っくぉ"
    , Rule "vva" "っゔぁ"
    , Rule "vvi" "っゔぃ"
    , Rule "vvu" "っゔ"
    , Rule "vve" "っゔぇ"
    , Rule "vvo" "っゔぉ"
    , Rule "vvya" "っゔゃ"
    , Rule "vvyi" "っゔぃ"
    , Rule "vvyu" "っゔゅ"
    , Rule "vvye" "っゔぇ"
    , Rule "vvyo" "っゔょ"
    , Rule "lla" "っぁ"
    , Rule "lli" "っぃ"
    , Rule "llu" "っぅ"
    , Rule "lle" "っぇ"
    , Rule "llo" "っぉ"
    , Rule "llyi" "っぃ"
    , Rule "llye" "っぇ"
    , Rule "llka" "っヵ"
    , Rule "llke" "っヶ"
    , Rule "lltu" "っっ"
    , Rule "llya" "っゃ"
    , Rule "llyu" "っゅ"
    , Rule "llyo" "っょ"
    , Rule "llwa" "っゎ"
    , Rule "xxn" "っん"
    , Rule "xxa" "っぁ"
    , Rule "xxi" "っぃ"
    , Rule "xxu" "っぅ"
    , Rule "xxe" "っぇ"
    , Rule "xxo" "っぉ"
    , Rule "xxyi" "っぃ"
    , Rule "xxye" "っぇ"
    , Rule "xxka" "っヵ"
    , Rule "xxke" "っヶ"
    , Rule "xxtu" "っっ"
    , Rule "xxya" "っゃ"
    , Rule "xxyu" "っゅ"
    , Rule "xxyo" "っょ"
    , Rule "xxwa" "っゎ"
    , Rule "kkya" "っきゃ"
    , Rule "kkyi" "っきぃ"
    , Rule "kkyu" "っきゅ"
    , Rule "kkye" "っきぇ"
    , Rule "kkyo" "っきょ"
    , Rule "kka" "っか"
    , Rule "kki" "っき"
    , Rule "kku" "っく"
    , Rule "kke" "っけ"
    , Rule "kko" "っこ"
    , Rule "kkwa" "っくぁ"
    , Rule "ggya" "っぎゃ"
    , Rule "ggyi" "っぎぃ"
    , Rule "ggyu" "っぎゅ"
    , Rule "ggye" "っぎぇ"
    , Rule "ggyo" "っぎょ"
    , Rule "gga" "っが"
    , Rule "ggi" "っぎ"
    , Rule "ggu" "っぐ"
    , Rule "gge" "っげ"
    , Rule "ggo" "っご"
    , Rule "ggwa" "っぐぁ"
    , Rule "ggwi" "っぐぃ"
    , Rule "ggwu" "っぐぅ"
    , Rule "ggwe" "っぐぇ"
    , Rule "ggwo" "っぐぉ"
    , Rule "ssya" "っしゃ"
    , Rule "ssyi" "っしぃ"
    , Rule "ssyu" "っしゅ"
    , Rule "ssye" "っしぇ"
    , Rule "ssyo" "っしょ"
    , Rule "ssha" "っしゃ"
    , Rule "sshi" "っし"
    , Rule "sshu" "っしゅ"
    , Rule "sshe" "っしぇ"
    , Rule "ssho" "っしょ"
    , Rule "ssa" "っさ"
    , Rule "ssi" "っし"
    , Rule "ssu" "っす"
    , Rule "sse" "っせ"
    , Rule "sso" "っそ"
    , Rule "zzya" "っじゃ"
    , Rule "zzyi" "っじぃ"
    , Rule "zzyu" "っじゅ"
    , Rule "zzye" "っじぇ"
    , Rule "zzyo" "っじょ"
    , Rule "zza" "っざ"
    , Rule "zzi" "っじ"
    , Rule "zzu" "っず"
    , Rule "zze" "っぜ"
    , Rule "zzo" "っぞ"
    , Rule "jja" "っじゃ"
    , Rule "jji" "っじ"
    , Rule "jju" "っじゅ"
    , Rule "jje" "っじぇ"
    , Rule "jjo" "っじょ"
    , Rule "jjya" "っじゃ"
    , Rule "jjyi" "っじぃ"
    , Rule "jjyu" "っじゅ"
    , Rule "jjye" "っじぇ"
    , Rule "jjyo" "っじょ"
    , Rule "ttya" "っちゃ"
    , Rule "ttyi" "っちぃ"
    , Rule "ttyu" "っちゅ"
    , Rule "ttye" "っちぇ"
    , Rule "ttyo" "っちょ"
    , Rule "ttsa" "っつぁ"
    , Rule "ttsi" "っつぃ"
    , Rule "ttse" "っつぇ"
    , Rule "ttso" "っつぉ"
    , Rule "ttha" "ってゃ"
    , Rule "tthi" "ってぃ"
    , Rule "tthu" "ってゅ"
    , Rule "tthe" "ってぇ"
    , Rule "ttho" "ってょ"
    , Rule "ttwa" "っとぁ"
    , Rule "ttwi" "っとぃ"
    , Rule "ttwu" "っとぅ"
    , Rule "ttwe" "っとぇ"
    , Rule "ttwo" "っとぉ"
    , Rule "tta" "った"
    , Rule "tti" "っち"
    , Rule "ttu" "っつ"
    , Rule "ttsu" "っつ"
    , Rule "tte" "って"
    , Rule "tto" "っと"
    , Rule "ddya" "っぢゃ"
    , Rule "ddyi" "っぢぃ"
    , Rule "ddyu" "っぢゅ"
    , Rule "ddye" "っぢぇ"
    , Rule "ddyo" "っぢょ"
    , Rule "ddha" "っでゃ"
    , Rule "ddhi" "っでぃ"
    , Rule "ddhu" "っでゅ"
    , Rule "ddhe" "っでぇ"
    , Rule "ddho" "っでょ"
    , Rule "ddwa" "っどぁ"
    , Rule "ddwi" "っどぃ"
    , Rule "ddwu" "っどぅ"
    , Rule "ddwe" "っどぇ"
    , Rule "ddwo" "っどぉ"
    , Rule "dda" "っだ"
    , Rule "ddi" "っぢ"
    , Rule "ddu" "っづ"
    , Rule "dde" "っで"
    , Rule "ddo" "っど"
    , Rule "hhya" "っひゃ"
    , Rule "hhyi" "っひぃ"
    , Rule "hhyu" "っひゅ"
    , Rule "hhye" "っひぇ"
    , Rule "hhyo" "っひょ"
    , Rule "hha" "っは"
    , Rule "hhi" "っひ"
    , Rule "hhu" "っふ"
    , Rule "hhe" "っへ"
    , Rule "hho" "っほ"
    , Rule "ffa" "っふぁ"
    , Rule "ffi" "っふぃ"
    , Rule "ffu" "っふ"
    , Rule "ffe" "っふぇ"
    , Rule "ffo" "っふぉ"
    , Rule "ffya" "っふゃ"
    , Rule "ffyu" "っふゅ"
    , Rule "ffyo" "っふょ"
    , Rule "ffu" "っふ"
    , Rule "bbya" "っびゃ"
    , Rule "bbyi" "っびぃ"
    , Rule "bbyu" "っびゅ"
    , Rule "bbye" "っびぇ"
    , Rule "bbyo" "っびょ"
    , Rule "bba" "っば"
    , Rule "bbi" "っび"
    , Rule "bbu" "っぶ"
    , Rule "bbe" "っべ"
    , Rule "bbo" "っぼ"
    , Rule "ppya" "っぴゃ"
    , Rule "ppyi" "っぴぃ"
    , Rule "ppyu" "っぴゅ"
    , Rule "ppye" "っぴぇ"
    , Rule "ppyo" "っぴょ"
    , Rule "ppa" "っぱ"
    , Rule "ppi" "っぴ"
    , Rule "ppu" "っぷ"
    , Rule "ppe" "っぺ"
    , Rule "ppo" "っぽ"
    , Rule "mmya" "っみゃ"
    , Rule "mmyi" "っみぃ"
    , Rule "mmyu" "っみゅ"
    , Rule "mmye" "っみぇ"
    , Rule "mmyo" "っみょ"
    , Rule "mma" "っま"
    , Rule "mmi" "っみ"
    , Rule "mmu" "っむ"
    , Rule "mme" "っめ"
    , Rule "mmo" "っも"
    , Rule "yye" "っいぇ"
    , Rule "yya" "っや"
    , Rule "yyu" "っゆ"
    , Rule "yyo" "っよ"
    , Rule "rrya" "っりゃ"
    , Rule "rryi" "っりぃ"
    , Rule "rryu" "っりゅ"
    , Rule "rrye" "っりぇ"
    , Rule "rryo" "っりょ"
    , Rule "rra" "っら"
    , Rule "rri" "っり"
    , Rule "rru" "っる"
    , Rule "rre" "っれ"
    , Rule "rro" "っろ"
    , Rule "wwu" "っう"
    , Rule "wwyi" "っゐ"
    , Rule "wwye" "っゑ"
    , Rule "wwa" "っわ"
    , Rule "wwi" "っうぃ"
    , Rule "wwe" "っうぇ"
    , Rule "wwo" "っを"
    , Rule "wwha" "っうぁ"
    , Rule "wwhi" "っうぃ"
    , Rule "wwhu" "っう"
    , Rule "wwhe" "っうぇ"
    , Rule "wwho" "っうぉ"
    , Rule "ccha" "っちゃ"
    , Rule "cchi" "っち"
    , Rule "cchu" "っちゅ"
    , Rule "cche" "っちぇ"
    , Rule "ccho" "っちょ"
    , Rule "ccya" "っちゃ"
    , Rule "ccyi" "っちぃ"
    , Rule "ccyu" "っちゅ"
    , Rule "ccye" "っちぇ"
    , Rule "ccyo" "っちょ"
    , Rule "cca" "っか"
    , Rule "cci" "っし"
    , Rule "ccu" "っく"
    , Rule "cce" "っせ"
    , Rule "cco" "っこ"
    , Rule "a" "a"
    , Rule "b" "b"
    , Rule "c" "c"
    , Rule "d" "d"
    , Rule "e" "e"
    , Rule "f" "f"
    , Rule "g" "g"
    , Rule "h" "h"
    , Rule "i" "i"
    , Rule "j" "j"
    , Rule "k" "k"
    , Rule "l" "l"
    , Rule "m" "m"
    , Rule "n" "n"
    , Rule "o" "o"
    , Rule "p" "p"
    , Rule "q" "q"
    , Rule "r" "r"
    , Rule "s" "s"
    , Rule "t" "t"
    , Rule "u" "u"
    , Rule "v" "v"
    , Rule "w" "w"
    , Rule "x" "x"
    , Rule "y" "y"
    , Rule "z" "z"
    , Rule "A" "A"
    , Rule "B" "B"
    , Rule "C" "C"
    , Rule "D" "D"
    , Rule "E" "E"
    , Rule "F" "F"
    , Rule "G" "G"
    , Rule "H" "H"
    , Rule "I" "I"
    , Rule "J" "J"
    , Rule "K" "K"
    , Rule "L" "L"
    , Rule "M" "M"
    , Rule "N" "N"
    , Rule "O" "O"
    , Rule "P" "P"
    , Rule "Q" "Q"
    , Rule "R" "R"
    , Rule "S" "S"
    , Rule "T" "T"
    , Rule "U" "U"
    , Rule "V" "V"
    , Rule "W" "W"
    , Rule "X" "X"
    , Rule "Y" "Y"
    , Rule "Z" "Z"
    , Rule "0" "0"
    , Rule "1" "1"
    , Rule "2" "2"
    , Rule "3" "3"
    , Rule "4" "4"
    , Rule "5" "5"
    , Rule "6" "6"
    , Rule "7" "7"
    , Rule "8" "8"
    , Rule "9" "9"
    , Rule "`" "`"
    , Rule "~" "~"
    , Rule "!" "!"
    , Rule "@" "@"
    , Rule "#" "#"
    , Rule "$" "$"
    , Rule "%" "%"
    , Rule "^" "^"
    , Rule "&" "&"
    , Rule "*" "*"
    , Rule "(" "("
    , Rule ")" ")"
    , Rule "-" "-"
    , Rule "_" "_"
    , Rule "=" "="
    , Rule "+" "+"
    , Rule "[" "["
    , Rule "]" "]"
    , Rule "{" "{"
    , Rule "}" "}"
    , Rule "\\" "\\"
    , Rule "|" "|"
    , Rule ";" ";"
    , Rule ":" ":"
    , Rule "'" "'"
    , Rule "\"" "\""
    , Rule "," ","
    , Rule "<" "<"
    , Rule "." "."
    , Rule ">" ">"
    , Rule "/" "/"
    , Rule "?" "?"
    , Rule " " " "
    ]


type alias State =
    { inputBuffer : String
    , tmpFixed : Maybe Rule
    , candidates : List Rule
    }


type alias Data =
    { fixedWords : String
    , restWords : String
    , state : State
    }


newData : String -> Data
newData words =
    Data "" words (State "" Nothing romanTable)


{-| 仮確定を確定させ次に進めたデータを返す
-}
nextData : Rule -> Data -> Data
nextData fixedRule data =
    Data
        (data.fixedWords ++ fixedRule.output)
        (String.dropLeft (String.length fixedRule.output) data.restWords)
        (State
            ""
            Nothing
            romanTable
        )


typeTo : String -> Data -> Maybe Data
typeTo input data =
    let
        sumInput =
            data.state.inputBuffer ++ input

        nextCandidates =
            List.filter (\r -> String.startsWith sumInput r.input) data.state.candidates

        acceptCandidates =
            List.filter (\r -> String.startsWith r.output data.restWords) nextCandidates

        tmpFixed =
            List.filter (\r -> (==) sumInput r.input) acceptCandidates |> List.head

        nl =
            List.length nextCandidates

        al =
            List.length acceptCandidates
    in
    if nl > 0 && al == 0 then
        -- 候補はあるが、正候補はない。ミス。
        Nothing

    else if nl == 0 && al == 0 then
        --  候補も正候補もない。
        case data.state.tmpFixed of
            Nothing ->
                -- 仮確定してるルールもないので受け付けない。ミス。
                Nothing

            Just tmp ->
                -- 仮確定してるルールはある(主にn)。仮確定を確定させ次に進めてしまう。
                typeTo input (nextData tmp data)

    else if al == 1 then
        case tmpFixed of
            Nothing ->
                Just (Data data.fixedWords data.restWords (State sumInput tmpFixed nextCandidates))

            Just tmp ->
                -- 1つに確定。
                Just (nextData tmp data)

    else
        -- 確定したものはあるが、まだ変化する可能性はある。
        Just (Data data.fixedWords data.restWords (State sumInput tmpFixed nextCandidates))
